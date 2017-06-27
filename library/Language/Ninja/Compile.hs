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
module Language.Ninja.Compile
  ( compile
  ) where

import           Control.Applicative          ((<|>))
import           Control.Arrow                (first)

import           Control.Lens.Getter          ((^.))
import           Control.Lens.Setter          ((.~))

import           Control.Exception            (Exception)
import           Control.Monad.Error.Class    (MonadError (..))

import           Data.Char                    (isSpace)
import           Data.Functor                 (void)
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Monoid                  (Endo (..), (<>))

import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.Aeson.Types             as Aeson

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC8

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC8

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T

import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM

import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS

import           Flow                         ((.>), (|>))

import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)

import qualified Data.Versions                as Ver

import qualified Language.Ninja.Errors        as Ninja
import           Language.Ninja.IR
                 (Build, Command, Dependency, DependencyType (..), Meta, Ninja,
                 Output, OutputType (..), Pool, ResponseFile, Rule,
                 SpecialDeps, Target)
import qualified Language.Ninja.Misc.Positive as Ninja
import qualified Language.Ninja.Parse         as Ninja

import qualified Language.Ninja.IR            as IR

import           Language.Ninja.AST           (FileText, PBuild, PNinja)
import qualified Language.Ninja.AST           as AST
import qualified Language.Ninja.AST.Env       as AST
import qualified Language.Ninja.AST.Expr      as AST
import qualified Language.Ninja.AST.Rule      as AST

-------------------------------------------------------------------------------

-- | Compile a 'PNinja' into a 'Ninja'.
compile :: forall m. (MonadError Ninja.CompileError m) => PNinja -> m Ninja
compile pninja = result
  where
    result :: m Ninja
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

    metaM :: m Meta
    metaM = do
      let getSpecial :: Text -> Maybe Text
          getSpecial name = HM.lookup name (pninja ^. AST.pninjaSpecials)

      let parseVersion :: Text -> m Ver.Version
          parseVersion = Ver.version
                         .> either Ninja.throwVersionParseFailure pure

      reqversion <- getSpecial "ninja_required_version"
                    |> fmap parseVersion
                    |> sequenceA
      builddir   <- getSpecial "builddir"
                    |> fmap IR.makePath
                    |> pure

      IR.makeMeta
        |> IR.metaReqVersion .~ reqversion
        |> IR.metaBuildDir   .~ builddir
        |> pure

    buildsM :: m (HashSet Build)
    buildsM = (pmultiples <> onHM (first HS.singleton) psingles)
              |> HM.toList |> mapM compileBuild |> fmap HS.fromList

    phonysM :: m (HashMap Target (HashSet Target))
    phonysM = HM.toList pphonys |> mapM compilePhony |> fmap HM.fromList

    defaultsM :: m (HashSet Target)
    defaultsM = HS.toList pdefaults |> mapM compileDefault |> fmap HS.fromList

    poolsM :: m (HashSet Pool)
    poolsM = HM.toList ppools |> mapM compilePool |> fmap HS.fromList

    compileBuild :: (HashSet FileText, PBuild) -> m Build
    compileBuild (outputs, pbuild) = do
      let pdeps         = pbuild ^. AST.pbuildDeps
      let normalDeps    = HS.toList (pdeps ^. AST.pdepsNormal)
      let implicitDeps  = HS.toList (pdeps ^. AST.pdepsImplicit)
      let orderOnlyDeps = HS.toList (pdeps ^. AST.pdepsOrderOnly)

      rule <- compileRule (outputs, pbuild)
      outs <- HS.toList outputs |> mapM compileOutput |> fmap HS.fromList
      deps <- let compileDep = flip (curry compileDependency)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (compileDep NormalDependency)    normalDeps
                 <*> mapM (compileDep ImplicitDependency)  implicitDeps
                 <*> mapM (compileDep OrderOnlyDependency) orderOnlyDeps

      IR.makeBuild rule
        |> IR.buildOuts .~ outs
        |> IR.buildDeps .~ deps
        |> pure

    compilePhony :: (Text, HashSet FileText)
                  -> m (Target, HashSet Target)
    compilePhony (name, deps) = do
      ename <- compileTarget name
      edeps <- HS.fromList <$> mapM compileTarget (HS.toList deps)
      pure (ename, edeps)

    compileDefault :: FileText -> m Target
    compileDefault = compileTarget

    compilePool :: (Text, Int) -> m Pool
    compilePool pair = case pair of
      ("console", 1) -> pure IR.makePoolConsole
      ("console", d) -> Ninja.throwInvalidPoolDepth d
      ("",        _) -> Ninja.throwEmptyPoolName
      (name,      d) -> do dp <- Ninja.makePositive d
                                 |> maybe (Ninja.throwInvalidPoolDepth d) pure
                           pure (IR.makePoolCustom name dp)

    compileRule :: (HashSet FileText, PBuild) -> m Rule
    compileRule (outputs, pbuild) = do
      (name, prule) <- lookupRule pbuild

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (Ninja.throwRuleLookupFailure var) pure

      let env = computeRuleEnv (outputs, pbuild) prule

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = AST.askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= compileCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = pbuild ^. AST.pbuildBind
                      in (HM.lookup "pool" buildBind <|> AST.askEnv env "pool")
                         |> fmap IR.parsePoolName
                         |> fromMaybe IR.makePoolNameDefault
                         |> pure
      depfile      <- lookupBind "depfile"
                      |> fmap (fmap IR.makePath)
      specialDeps  <- let prefix = "msvc_deps_prefix"
                      in ((,) <$> lookupBind "deps" <*> lookupBind prefix)
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

    compileSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe SpecialDeps)
    compileSpecialDeps = (\case (Nothing,     _) -> pure Nothing
                                (Just "gcc",  m) -> goGCC  m
                                (Just "msvc", m) -> goMSVC m
                                (Just d,      _) -> Ninja.throwUnknownDeps d)
      where
        goGCC  Nothing  = pure (Just IR.makeSpecialDepsGCC)
        goGCC  (Just _) = Ninja.throwUnexpectedMSVCPrefix "gcc"

        goMSVC m        = pure (Just (IR.makeSpecialDepsMSVC m))

    compileResponseFile :: (Text, Text) -> m ResponseFile
    compileResponseFile (file, content) = do
      let path = IR.makePath file
      pure (IR.makeResponseFile path content)

    compileTarget :: Text -> m Target
    compileTarget = IR.makeTarget .> pure

    compileOutput :: Text -> m Output
    compileOutput name = do
      target <- compileTarget name
      pure (IR.makeOutput target ExplicitOutput)

    compileDependency :: (Text, DependencyType) -> m Dependency
    compileDependency (name, ty) = do
      target <- compileTarget name
      pure (IR.makeDependency target ty)

    compileCommand :: Text -> m Command
    compileCommand = IR.makeCommand .> pure

    lookupRule :: PBuild -> m (Text, AST.Rule)
    lookupRule pbuild = do
      let name = pbuild ^. AST.pbuildRule
      prule <- HM.lookup name prules
               |> maybe (Ninja.throwBuildRuleNotFound name) pure
      pure (name, prule)

    computeRuleEnv :: (HashSet Text, PBuild)
                   -> AST.Rule
                   -> AST.Env Text Text
    computeRuleEnv (outs, pbuild) prule = do
      let deps = pbuild ^. AST.pbuildDeps . AST.pdepsNormal

      let composeList :: [a -> a] -> (a -> a)
          composeList = map Endo .> mconcat .> appEndo

          quote :: Text -> Text
          quote x | T.any isSpace x = mconcat ["\"", x, "\""]
          quote x                   = x

          pbuildBind = pbuild ^. AST.pbuildBind

      -- the order of adding new environment variables matters
      AST.scopeEnv (pbuild ^. AST.pbuildEnv)
        |> AST.addEnv "out"        (T.unwords (map quote (HS.toList outs)))
        |> AST.addEnv "in"         (T.unwords (map quote (HS.toList deps)))
        |> AST.addEnv "in_newline" (T.unlines (HS.toList deps))
        |> composeList (map (uncurry AST.addEnv) (HM.toList pbuildBind))
        |> AST.addBinds (HM.toList (prule ^. AST.ruleBind))

    prules     :: HashMap Text AST.Rule
    psingles   :: HashMap FileText PBuild
    pmultiples :: HashMap (HashSet FileText) PBuild
    pphonys    :: HashMap Text (HashSet FileText)
    pdefaults  :: HashSet FileText
    ppools     :: HashMap Text Int
    prules     = pninja ^. AST.pninjaRules
    psingles   = pninja ^. AST.pninjaSingles
    pmultiples = pninja ^. AST.pninjaMultiples
    pphonys    = pninja ^. AST.pninjaPhonys
    pdefaults  = pninja ^. AST.pninjaDefaults
    ppools     = pninja ^. AST.pninjaPools

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
