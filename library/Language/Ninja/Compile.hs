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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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

import           Control.Applicative        ((<|>))
import           Control.Arrow              (first)

import           Control.Lens.Getter        (view, (^.))
import           Control.Lens.Setter        (set, (.~))

import           Control.Exception          (Exception)
import           Control.Monad.Error.Class  (MonadError (..))

import           Data.Char                  (isSpace)
import           Data.Functor               (void)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                (Endo (..), (<>))

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.Aeson.Types           as Aeson

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC8

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Flow                       ((.>), (|>))

import           Data.Hashable              (Hashable)
import           GHC.Generics               (Generic)

import qualified Data.Versions              as Ver

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Err
import qualified Language.Ninja.IR          as IR
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Parser      as Parser

-------------------------------------------------------------------------------

-- | Compile an parsed Ninja file into a intermediate representation.
--
--   @since 0.1.0
compile :: forall m ann. (MonadError Err.CompileError m)
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
        |> IR.ninjaMeta     .~ meta
        |> IR.ninjaBuilds   .~ builds
        |> IR.ninjaPhonys   .~ phonys
        |> IR.ninjaDefaults .~ defaults
        |> IR.ninjaPools    .~ pools
        |> pure

    metaM :: m IR.Meta
    metaM = do
      let getSpecial :: Text -> Maybe Text
          getSpecial name = HM.lookup name (ast ^. AST.ninjaSpecials)

      let parseVersion :: Text -> m Ver.Version
          parseVersion = Ver.version
                         .> either Err.throwVersionParseFailure pure

      reqversion <- getSpecial "ninja_required_version"
                    |> fmap parseVersion
                    |> sequenceA
      builddir   <- getSpecial "builddir"
                    |> fmap Misc.makePath
                    |> pure

      IR.makeMeta
        |> IR.metaReqVersion .~ reqversion
        |> IR.metaBuildDir   .~ builddir
        |> pure

    buildsM :: m (HashSet IR.Build)
    buildsM = (pmultiples <> onHM (first HS.singleton) psingles)
              |> HM.toList |> mapM compileBuild |> fmap HS.fromList

    phonysM :: m (HashMap IR.Target (HashSet IR.Target))
    phonysM = HM.toList pphonys |> mapM compilePhony |> fmap HM.fromList

    defaultsM :: m (HashSet IR.Target)
    defaultsM = HS.toList pdefaults |> mapM compileDefault |> fmap HS.fromList

    poolsM :: m (HashSet IR.Pool)
    poolsM = HM.toList ppools |> mapM compilePool |> fmap HS.fromList

    compileBuild :: (HashSet Text, AST.Build ann) -> m IR.Build
    compileBuild (outputs, buildAST) = do
      let depsAST       = buildAST ^. AST.buildDeps
      let normalDeps    = HS.toList (depsAST ^. AST.depsNormal)
      let implicitDeps  = HS.toList (depsAST ^. AST.depsImplicit)
      let orderOnlyDeps = HS.toList (depsAST ^. AST.depsOrderOnly)

      outs <- HS.toList outputs |> mapM compileOutput |> fmap HS.fromList
      rule <- compileRule (outs, buildAST)
      deps <- let compileDep = flip (curry compileDependency)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (compileDep IR.NormalDependency)    normalDeps
                 <*> mapM (compileDep IR.ImplicitDependency)  implicitDeps
                 <*> mapM (compileDep IR.OrderOnlyDependency) orderOnlyDeps

      IR.makeBuild rule
        |> IR.buildOuts .~ outs
        |> IR.buildDeps .~ deps
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
      ("console", d) -> Err.throwInvalidPoolDepth d
      ("",        _) -> Err.throwEmptyPoolName
      (name,      d) -> do dp <- Misc.makePositive d
                                 |> maybe (Err.throwInvalidPoolDepth d) pure
                           pure (IR.makePoolCustom name dp)

    compileRule :: (HashSet IR.Output, AST.Build ann) -> m IR.Rule
    compileRule (outputs, buildAST) = do
      (name, prule) <- lookupRule buildAST

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (Err.throwRuleLookupFailure var) pure

      let env = computeRuleEnv (outputs, buildAST) prule

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = AST.askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= compileCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = buildAST ^. AST.buildBind
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
        |> IR.ruleDescription  .~ description
        |> IR.rulePool         .~ pool
        |> IR.ruleDepfile      .~ depfile
        |> IR.ruleSpecialDeps  .~ specialDeps
        |> IR.ruleGenerator    .~ generator
        |> IR.ruleRestat       .~ restat
        |> IR.ruleResponseFile .~ responseFile
        |> pure

    compileSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe IR.SpecialDeps)
    compileSpecialDeps = (\case (Nothing,     _) -> pure Nothing
                                (Just "gcc",  m) -> goGCC  m
                                (Just "msvc", m) -> goMSVC m
                                (Just d,      _) -> Err.throwUnknownDeps d)
      where
        goGCC  Nothing  = pure (Just IR.makeSpecialDepsGCC)
        goGCC  (Just _) = Err.throwUnexpectedMSVCPrefix "gcc"

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
      pure (IR.makeOutput target IR.ExplicitOutput)

    compileDependency :: (Text, IR.DependencyType) -> m IR.Dependency
    compileDependency (name, ty) = do
      target <- compileTarget name
      pure (IR.makeDependency target ty)

    compileCommand :: Text -> m Misc.Command
    compileCommand = Misc.makeCommand .> pure

    lookupRule :: AST.Build ann -> m (Text, AST.Rule ann)
    lookupRule buildAST = do
      let name = buildAST ^. AST.buildRule
      prule <- HM.lookup name prules
               |> maybe (Err.throwBuildRuleNotFound name) pure
      pure (name, prule)

    computeRuleEnv :: (HashSet IR.Output, AST.Build ann)
                   -> AST.Rule ann
                   -> AST.Env Text Text
    computeRuleEnv (outs, buildAST) prule = do
      let isExplicitOut out = (out ^. IR.outputType) == IR.ExplicitOutput

      let explicitOuts = HS.toList outs
                         |> filter isExplicitOut
                         |> map (view (IR.outputTarget . IR.targetText))

      let explicitDeps = [ buildAST ^. AST.buildDeps . AST.depsNormal
                         , buildAST ^. AST.buildDeps . AST.depsOrderOnly
                         ] |> mconcat |> HS.toList

      let composeList :: [a -> a] -> (a -> a)
          composeList = map Endo .> mconcat .> appEndo

      let quote :: Text -> Text
          quote x | T.any isSpace x = mconcat ["\"", x, "\""]
          quote x                   = x

      let bindings = buildAST ^. AST.buildBind

      -- the order of adding new environment variables matters
      AST.scopeEnv (buildAST ^. AST.buildEnv)
        |> AST.addEnv "out"        (T.unwords (map quote explicitOuts))
        |> AST.addEnv "in"         (T.unwords (map quote explicitDeps))
        |> AST.addEnv "in_newline" (T.unlines explicitDeps)
        |> composeList (map (uncurry AST.addEnv) (HM.toList bindings))
        |> AST.addBinds (HM.toList (prule ^. AST.ruleBind))

    prules     :: HashMap Text (AST.Rule ann)
    psingles   :: HashMap Text (AST.Build ann)
    pmultiples :: HashMap (HashSet Text) (AST.Build ann)
    pphonys    :: HashMap Text (HashSet Text)
    pdefaults  :: HashSet Text
    ppools     :: HashMap Text Int
    prules     = ast ^. AST.ninjaRules
    psingles   = ast ^. AST.ninjaSingles
    pmultiples = ast ^. AST.ninjaMultiples
    pphonys    = ast ^. AST.ninjaPhonys
    pdefaults  = ast ^. AST.ninjaDefaults
    ppools     = ast ^. AST.ninjaPools

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
