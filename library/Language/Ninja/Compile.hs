-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Compile.hs
--
-- License:
--     Copyright 2017 Awake Security
--
--     Licensed under the Apache License, Version 2.0 (the "License");
--     you may not use this file except in compliance with the License.
--     You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--     Unless required by applicable law or agreed to in writing, software
--     distributed under the License is distributed on an "AS IS" BASIS,
--     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--     See the License for the specific language governing permissions and
--     limitations under the License.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
--   Module      : Language.Ninja.Compile
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Compile a parsed Ninja file into an intermediate representation.
--
--   @since 0.1.0
module Language.Ninja.Compile
  ( compile
  ) where

import           Control.Applicative       ((<|>))
import           Control.Arrow             (first)

import qualified Control.Lens              as Lens

import           Control.Monad.Error.Class (MonadError (..))

import           Data.Char                 (isSpace)
import           Data.Maybe                (fromMaybe, isJust)
import           Data.Monoid               (Endo (..), (<>))

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS

import           Flow                      ((.>), (|>))

import           Data.Hashable             (Hashable)

import qualified Data.Versions             as Ver

import qualified Language.Ninja.AST        as AST
import qualified Language.Ninja.Errors     as Errors
import qualified Language.Ninja.IR         as IR
import qualified Language.Ninja.Misc       as Misc

-------------------------------------------------------------------------------

-- | Compile an parsed Ninja file into a intermediate representation.
--
--   @since 0.1.0
compile :: forall m ann. (MonadError Errors.CompileError m)
        => AST.Ninja ann -> m IR.Ninja
compile ast = result
  where
    result :: m IR.Ninja
    result = do
      meta     <- metaM
      builds   <- buildsM
      phonys   <- phonysM
      defaults <- defaultsM
      pools    <- poolsM

      IR.makeNinja
        |> Lens.set IR.ninjaMeta     meta
        |> Lens.set IR.ninjaBuilds   builds
        |> Lens.set IR.ninjaPhonys   phonys
        |> Lens.set IR.ninjaDefaults defaults
        |> Lens.set IR.ninjaPools    pools
        |> pure

    metaM :: m IR.Meta
    metaM = do
      let getSpecial :: Text -> Maybe Text
          getSpecial name = HM.lookup name (Lens.view AST.ninjaSpecials ast)

      let parseVersion :: Text -> m Ver.Version
          parseVersion = Ver.version
                         .> either Errors.throwVersionParseFailure pure

      reqversion <- getSpecial "ninja_required_version"
                    |> fmap parseVersion
                    |> sequenceA
      builddir   <- getSpecial "builddir"
                    |> fmap Misc.makePath
                    |> pure

      IR.makeMeta
        |> Lens.set IR.metaReqVersion reqversion
        |> Lens.set IR.metaBuildDir   builddir
        |> pure

    buildsM :: m (HashSet IR.Build)
    buildsM = (multiplesAST <> onHM (first HS.singleton) singlesAST)
              |> HM.toList |> mapM compileBuild |> fmap HS.fromList

    phonysM :: m (HashMap IR.Target (HashSet IR.Target))
    phonysM = HM.toList phonysAST |> mapM compilePhony |> fmap HM.fromList

    defaultsM :: m (HashSet IR.Target)
    defaultsM = HS.toList defaultsAST |> mapM compileDefault |> fmap HS.fromList

    poolsM :: m (HashSet IR.Pool)
    poolsM = HM.toList poolsAST |> mapM compilePool |> fmap HS.fromList

    compileBuild :: (HashSet Text, AST.Build ann) -> m IR.Build
    compileBuild (outputs, buildAST) = do
      let depsAST       = Lens.view AST.buildDeps buildAST
      let normalDeps    = HS.toList (Lens.view AST.depsNormal    depsAST)
      let implicitDeps  = HS.toList (Lens.view AST.depsImplicit  depsAST)
      let orderOnlyDeps = HS.toList (Lens.view AST.depsOrderOnly depsAST)

      outs <- HS.toList outputs |> mapM compileOutput |> fmap HS.fromList
      rule <- compileRule (outs, buildAST)
      deps <- let compileDep ty dep = compileDependency (dep, ty)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (compileDep IR.NormalDependency)    normalDeps
                 <*> mapM (compileDep IR.NormalDependency)    implicitDeps
                 <*> mapM (compileDep IR.OrderOnlyDependency) orderOnlyDeps

      IR.makeBuild rule
        |> Lens.set IR.buildOuts outs
        |> Lens.set IR.buildDeps deps
        |> pure

    compilePhony :: (Text, HashSet Text)
                  -> m (IR.Target, HashSet IR.Target)
    compilePhony (name, deps) = do
      ename <- compileTarget name
      edeps <- HS.fromList <$> mapM compileTarget (HS.toList deps)
      pure (ename, edeps)

    compileDefault :: Text -> m IR.Target
    compileDefault = compileTarget

    compilePool :: (Text, Int) -> m IR.Pool
    compilePool pair = case pair of
      ("console", 1) -> pure IR.makePoolConsole
      ("console", d) -> Errors.throwInvalidPoolDepth d
      ("",        _) -> Errors.throwEmptyPoolName
      (name,      d) -> do dp <- Misc.makePositive d
                                 |> maybe (Errors.throwInvalidPoolDepth d) pure
                           pure (IR.makePoolCustom name dp)

    compileRule :: (HashSet IR.Output, AST.Build ann) -> m IR.Rule
    compileRule (outputs, buildAST) = do
      (name, ruleAST) <- lookupRule buildAST

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (Errors.throwRuleLookupFailure var) pure

      let env = computeRuleEnv (outputs, buildAST) ruleAST

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = AST.askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= compileCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = Lens.view AST.buildBind buildAST
                      in (HM.lookup "pool" buildBind <|> AST.askEnv env "pool")
                         |> fromMaybe ""
                         |> IR.parsePoolName
                         |> pure
      depfile      <- lookupBind "depfile"
                      |> fmap (fmap Misc.makePath)
      specialDeps  <- let lookupPrefix = lookupBind "msvc_deps_prefix"
                      in ((,) <$> lookupBind "deps" <*> lookupPrefix)
                         >>= compileSpecialDeps
      generator    <- isJust <$> lookupBind "generator"
      restat       <- isJust <$> lookupBind "restat"
      responseFile <- let (rsp, rspcontent) = ("rspfile", "rspfile_content")
                      in ((,) <$> lookupBind rsp <*> lookupBind rspcontent)
                         >>= (\(ma, mb) -> pure ((,) <$> ma <*> mb))
                         >>= fmap compileResponseFile .> sequenceA

      IR.makeRule name command
        |> Lens.set IR.ruleDescription  description
        |> Lens.set IR.rulePool         pool
        |> Lens.set IR.ruleDepfile      depfile
        |> Lens.set IR.ruleSpecialDeps  specialDeps
        |> Lens.set IR.ruleGenerator    generator
        |> Lens.set IR.ruleRestat       restat
        |> Lens.set IR.ruleResponseFile responseFile
        |> pure

    compileSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe IR.SpecialDeps)
    compileSpecialDeps = (\case (Nothing,     _) -> pure Nothing
                                (Just "gcc",  m) -> goGCC  m
                                (Just "msvc", m) -> goMSVC m
                                (Just d,      _) -> Errors.throwUnknownDeps d)
      where
        goGCC  Nothing  = pure (Just IR.makeSpecialDepsGCC)
        goGCC  (Just _) = Errors.throwUnexpectedMSVCPrefix "gcc"

        goMSVC (Just m) = pure (Just (IR.makeSpecialDepsMSVC m))
        goMSVC Nothing  = pure (Just (IR.makeSpecialDepsMSVC defaultPrefix))

        defaultPrefix = "Note: including file: "

    compileResponseFile :: (Text, Text) -> m IR.ResponseFile
    compileResponseFile (file, content) = do
      let path = Misc.makePath file
      pure (IR.makeResponseFile path content)

    compileTarget :: Text -> m IR.Target
    compileTarget = IR.makeTarget .> pure

    compileOutput :: Text -> m IR.Output
    compileOutput name = do
      target <- compileTarget name
      pure (IR.makeOutput target)

    compileDependency :: (Text, IR.DependencyType) -> m IR.Dependency
    compileDependency (name, ty) = do
      target <- compileTarget name
      pure (IR.makeDependency target ty)

    compileCommand :: Text -> m Misc.Command
    compileCommand = Misc.makeCommand .> pure

    lookupRule :: AST.Build ann -> m (Text, AST.Rule ann)
    lookupRule buildAST = do
      let name = Lens.view AST.buildRule buildAST
      ruleAST <- HM.lookup name rulesAST
                 |> maybe (Errors.throwBuildRuleNotFound name) pure
      pure (name, ruleAST)

    computeRuleEnv :: (HashSet IR.Output, AST.Build ann)
                   -> AST.Rule ann
                   -> AST.Env Text Text
    computeRuleEnv (outs, buildAST) ruleAST = do
      let depsAST = Lens.view AST.buildDeps buildAST

      -- FIXME: properly handle implicit/explicit outputs here

      let isExplicitOut _ = True

      let explicitOuts = HS.toList outs
                         |> filter isExplicitOut
                         |> map (Lens.view (IR.outputTarget . IR.targetText))

      let explicitDeps = [ Lens.view AST.depsNormal    depsAST
                         , Lens.view AST.depsOrderOnly depsAST
                         ] |> mconcat |> HS.toList

      let composeList :: [a -> a] -> (a -> a)
          composeList = map Endo .> mconcat .> appEndo

      let quote :: Text -> Text
          quote x | Text.any isSpace x = mconcat ["\"", x, "\""]
          quote x                      = x

      let bindings = Lens.view AST.buildBind buildAST

      -- the order of adding new environment variables matters
      AST.scopeEnv (Lens.view AST.buildEnv buildAST)
        |> AST.addEnv "out"        (Text.unwords (map quote explicitOuts))
        |> AST.addEnv "in"         (Text.unwords (map quote explicitDeps))
        |> AST.addEnv "in_newline" (Text.unlines explicitDeps)
        |> composeList (map (uncurry AST.addEnv) (HM.toList bindings))
        |> AST.addBinds (HM.toList (Lens.view AST.ruleBind ruleAST))

    rulesAST     :: HashMap Text (AST.Rule ann)
    singlesAST   :: HashMap Text (AST.Build ann)
    multiplesAST :: HashMap (HashSet Text) (AST.Build ann)
    phonysAST    :: HashMap Text (HashSet Text)
    defaultsAST  :: HashSet Text
    poolsAST     :: HashMap Text Int
    rulesAST     = Lens.view AST.ninjaRules     ast
    singlesAST   = Lens.view AST.ninjaSingles   ast
    multiplesAST = Lens.view AST.ninjaMultiples ast
    phonysAST    = Lens.view AST.ninjaPhonys    ast
    defaultsAST  = Lens.view AST.ninjaDefaults  ast
    poolsAST     = Lens.view AST.ninjaPools     ast

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
