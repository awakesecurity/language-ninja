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
--   FIXME: doc
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

import           Language.Ninja.AST
                 (Build, Command, Dependency, DependencyType (..), Meta, Ninja,
                 Output, OutputType (..), Pool, ResponseFile, Rule,
                 SpecialDeps, Target)
import qualified Language.Ninja.AST           as Ninja
import           Language.Ninja.Env           (askEnv)
import qualified Language.Ninja.Errors        as Ninja
import qualified Language.Ninja.Misc.Positive as Ninja
import qualified Language.Ninja.Parse         as Ninja
import           Language.Ninja.Types
                 (Env, FileText, PBuild, PNinja, PRule)
import qualified Language.Ninja.Types         as Ninja

-------------------------------------------------------------------------------

-- | FIXME: doc
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

      Ninja.makeNinja
        |> Ninja.ninjaMeta     .~ meta
        |> Ninja.ninjaBuilds   .~ builds
        |> Ninja.ninjaPhonys   .~ phonys
        |> Ninja.ninjaDefaults .~ defaults
        |> Ninja.ninjaPools    .~ pools
        |> pure

    metaM :: m Meta
    metaM = do
      let getSpecial :: Text -> Maybe Text
          getSpecial name = HM.lookup name (pninja ^. Ninja.pninjaSpecials)

      let parseVersion :: Text -> m Ver.Version
          parseVersion = Ver.version
                         .> either Ninja.throwVersionParseFailure pure

      reqversion <- getSpecial "ninja_required_version"
                    |> fmap parseVersion
                    |> sequenceA
      builddir   <- getSpecial "builddir"
                    |> fmap Ninja.makePath
                    |> pure

      Ninja.makeMeta
        |> Ninja.metaReqVersion .~ reqversion
        |> Ninja.metaBuildDir   .~ builddir
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
      let pdepsNormal    = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsNormal
      let pdepsImplicit  = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsImplicit
      let pdepsOrderOnly = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsOrderOnly
      let normalDeps     = HS.toList pdepsNormal
      let implicitDeps   = HS.toList pdepsImplicit
      let orderOnlyDeps  = HS.toList pdepsOrderOnly

      rule <- compileRule (outputs, pbuild)
      outs <- HS.toList outputs |> mapM compileOutput |> fmap HS.fromList
      deps <- let compileDep = flip (curry compileDependency)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (compileDep NormalDependency)    normalDeps
                 <*> mapM (compileDep ImplicitDependency)  implicitDeps
                 <*> mapM (compileDep OrderOnlyDependency) orderOnlyDeps

      Ninja.makeBuild rule
        |> Ninja.buildOuts .~ outs
        |> Ninja.buildDeps .~ deps
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
      ("console", 1) -> pure Ninja.makePoolConsole
      ("console", d) -> Ninja.throwInvalidPoolDepth d
      ("",        _) -> Ninja.throwEmptyPoolName
      (name,      d) -> do dp <- Ninja.makePositive d
                                 |> maybe (Ninja.throwInvalidPoolDepth d) pure
                           pure (Ninja.makePoolCustom name dp)

    compileRule :: (HashSet FileText, PBuild) -> m Rule
    compileRule (outputs, pbuild) = do
      (name, prule) <- lookupRule pbuild

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (Ninja.throwRuleLookupFailure var) pure

      let env = computeRuleEnv (outputs, pbuild) prule

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= compileCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = pbuild ^. Ninja.pbuildBind
                      in (HM.lookup "pool" buildBind <|> askEnv env "pool")
                         |> fmap Ninja.parsePoolName
                         |> fromMaybe Ninja.makePoolNameDefault
                         |> pure
      depfile      <- lookupBind "depfile"
                      |> fmap (fmap Ninja.makePath)
      specialDeps  <- let prefix = "msvc_deps_prefix"
                      in ((,) <$> lookupBind "deps" <*> lookupBind prefix)
                         >>= compileSpecialDeps
      generator    <- isJust <$> lookupBind "generator"
      restat       <- isJust <$> lookupBind "restat"
      responseFile <- let (rsp, rspcontent) = ("rspfile", "rspfile_content")
                      in ((,) <$> lookupBind rsp <*> lookupBind rspcontent)
                         >>= (\(ma, mb) -> pure ((,) <$> ma <*> mb))
                         >>= fmap compileResponseFile .> sequenceA

      Ninja.makeRule name command
        |> Ninja.ruleDescription  .~ description
        |> Ninja.rulePool         .~ pool
        |> Ninja.ruleDepfile      .~ depfile
        |> Ninja.ruleSpecialDeps  .~ specialDeps
        |> Ninja.ruleGenerator    .~ generator
        |> Ninja.ruleRestat       .~ restat
        |> Ninja.ruleResponseFile .~ responseFile
        |> pure

    compileSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe SpecialDeps)
    compileSpecialDeps = go
      where
        go (Nothing,     _) = pure Nothing
        go (Just "gcc",  _) = pure (Just Ninja.makeSpecialDepsGCC)
        go (Just "msvc", m) = pure (Just (Ninja.makeSpecialDepsMSVC m))
        go (Just owise,  _) = Ninja.throwUnknownDeps owise

    compileResponseFile :: (Text, Text) -> m ResponseFile
    compileResponseFile (file, content)
      = pure (Ninja.makeResponseFile (Ninja.makePath file) content)

    compileTarget :: Text -> m Target
    compileTarget = Ninja.makeTarget .> pure

    compileOutput :: Text -> m Output
    compileOutput name = do
      target <- compileTarget name
      pure (Ninja.makeOutput target ExplicitOutput)

    compileDependency :: (Text, DependencyType) -> m Dependency
    compileDependency (name, ty) = do
      target <- compileTarget name
      pure (Ninja.makeDependency target ty)

    compileCommand :: Text -> m Command
    compileCommand = Ninja.makeCommand .> pure

    lookupRule :: PBuild -> m (Text, PRule)
    lookupRule pbuild = do
      let name = pbuild ^. Ninja.pbuildRule
      prule <- HM.lookup name prules
               |> maybe (Ninja.throwBuildRuleNotFound name) pure
      pure (name, prule)

    computeRuleEnv :: (HashSet Text, PBuild)
                   -> PRule
                   -> Env Text Text
    computeRuleEnv (outs, pbuild) prule = do
      let deps = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsNormal

      let composeList :: [a -> a] -> (a -> a)
          composeList = map Endo .> mconcat .> appEndo

          quote :: Text -> Text
          quote x | T.any isSpace x = mconcat ["\"", x, "\""]
          quote x                   = x

          pbuildBind = pbuild ^. Ninja.pbuildBind

      -- the order of adding new environment variables matters
      Ninja.scopeEnv (pbuild ^. Ninja.pbuildEnv)
        |> Ninja.addEnv "out"        (T.unwords (map quote (HS.toList outs)))
        |> Ninja.addEnv "in"         (T.unwords (map quote (HS.toList deps)))
        |> Ninja.addEnv "in_newline" (T.unlines (HS.toList deps))
        |> composeList (map (uncurry Ninja.addEnv) (HM.toList pbuildBind))
        |> Ninja.addBinds (HM.toList (prule ^. Ninja.pruleBind))

    prules     :: HashMap Text PRule
    psingles   :: HashMap FileText PBuild
    pmultiples :: HashMap (HashSet FileText) PBuild
    pphonys    :: HashMap Text (HashSet FileText)
    pdefaults  :: HashSet FileText
    ppools     :: HashMap Text Int
    prules     = pninja ^. Ninja.pninjaRules
    psingles   = pninja ^. Ninja.pninjaSingles
    pmultiples = pninja ^. Ninja.pninjaMultiples
    pphonys    = pninja ^. Ninja.pninjaPhonys
    pdefaults  = pninja ^. Ninja.pninjaDefaults
    ppools     = pninja ^. Ninja.pninjaPools

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
