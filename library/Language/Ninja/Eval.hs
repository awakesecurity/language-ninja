-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Eval.hs
--
-- License:
--     Copyright 2017 Awake Networks
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

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Module      : Language.Ninja.Eval
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter

import           Control.Exception          hiding (evaluate)
import           Control.Monad.Catch
import           Control.Monad.Except

import           Data.Char
import           Data.Either
import           Data.Maybe
import           Data.Monoid

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

import           Flow

import           Data.Hashable              (Hashable)
import           GHC.Generics               (Generic)

import           Language.Ninja.AST
import           Language.Ninja.Env
import qualified Language.Ninja.Parse       as Ninja
import           Language.Ninja.Types

--------------------------------------------------------------------------------

debugNinja :: IO PNinja
debugNinja = Ninja.parse "data/build.ninja"

testEval :: IO ()
testEval = void $ do
  pninja <- debugNinja
  ninja <- evaluate pninja
  LBSC8.putStrLn (Aeson.encodePretty ninja)
  -- print ninja

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvaluationError
  = MkEvaluationError
    { _evalErrorMessage :: !Text
    }
  deriving (Eq, Show, Generic)

-- FIXME: more granular exception type
-- FIXME: split off into Errors.hs

instance Exception EvaluationError

throwEvaluationError :: (MonadThrow m) => Text -> m a
throwEvaluationError msg = throwM (MkEvaluationError msg)

--------------------------------------------------------------------------------

-- | FIXME: doc
evaluate :: forall m. (MonadThrow m) => PNinja -> m Ninja
evaluate pninja = result
  where
    result :: m Ninja
    result = do
      meta     <- metaM
      builds   <- buildsM
      phonys   <- phonysM
      defaults <- defaultsM
      pools    <- poolsM
      pure (makeNinja
            & ninjaMeta     .~ meta
            & ninjaBuilds   .~ builds
            & ninjaPhonys   .~ phonys
            & ninjaDefaults .~ defaults
            & ninjaPools    .~ pools)

    metaM :: m Meta
    metaM = do
      -- FIXME: implement this
      pure makeMeta

    buildsM :: m (HashSet Build)
    buildsM = (pmultiples <> onHM (first HS.singleton) psingles)
              |> HM.toList |> mapM evaluateBuild |> fmap HS.fromList

    phonysM :: m (HashMap Target (HashSet Target))
    phonysM = HM.toList pphonys |> mapM evaluatePhony |> fmap HM.fromList

    defaultsM :: m (HashSet Target)
    defaultsM = HS.toList pdefaults |> mapM evaluateDefault |> fmap HS.fromList

    poolsM :: m (HashSet Pool)
    poolsM = HM.toList ppools |> mapM evaluatePool |> fmap HS.fromList

    evaluateBuild :: (HashSet FileText, PBuild) -> m Build
    evaluateBuild (outputs, pbuild) = do
      let normalDeps    = HS.toList (pbuild ^. pbuildDeps . pdepsNormal)
      let implicitDeps  = HS.toList (pbuild ^. pbuildDeps . pdepsImplicit)
      let orderOnlyDeps = HS.toList (pbuild ^. pbuildDeps . pdepsOrderOnly)

      rule <- evaluateRule (outputs, pbuild)
      outs <- HS.toList outputs |> mapM evaluateOutput |> fmap HS.fromList
      deps <- let evalDep = flip (curry evaluateDependency)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (evalDep NormalDependency)    normalDeps
                 <*> mapM (evalDep ImplicitDependency)  implicitDeps
                 <*> mapM (evalDep OrderOnlyDependency) orderOnlyDeps

      pure (makeBuild rule
            |> buildOuts .~ outs
            |> buildDeps .~ deps)

    evaluatePhony :: (Text, HashSet FileText)
                  -> m (Target, HashSet Target)
    evaluatePhony (name, deps) = do
      ename <- evaluateTarget name
      edeps <- HS.fromList <$> mapM evaluateTarget (HS.toList deps)
      pure (ename, edeps)

    evaluateDefault :: FileText -> m Target
    evaluateDefault = evaluateTarget

    evaluatePool :: (Text, Int) -> m Pool
    evaluatePool ("console", 1) = pure poolConsole
    evaluatePool ("console", d) = ["Invalid pool depth for console: ", show d]
                                  |> mconcat |> T.pack |> throwEvaluationError
    evaluatePool ("",        _) = ["Pool name is an empty string!"]
                                  |> mconcat |> T.pack |> throwEvaluationError
    evaluatePool (name,  depth) = pure (poolCustom name depth)

    evaluateRule :: (HashSet FileText, PBuild) -> m Rule
    evaluateRule (outputs, pbuild) = do
      (name, prule) <- lookupRule pbuild

      let throwLookupError :: Text -> m a
          throwLookupError v = ["Lookup failed on rule variable: ", v]
                               |> mconcat |> throwEvaluationError

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (throwLookupError var) pure

      let env = computeRuleEnv (outputs, pbuild) prule

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= evaluateCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = pbuild ^. pbuildBind
                      in (HM.lookup "pool" buildBind <|> askEnv env "pool")
                         |> fmap parsePoolName
                         |> fromMaybe poolNameDefault
                         |> pure
      depfile      <- lookupBind "depfile"
                      |> fmap (fmap makePath)
      specialDeps  <- let prefix = "msvc_deps_prefix"
                      in ((,) <$> lookupBind "deps" <*> lookupBind prefix)
                         >>= evaluateSpecialDeps
      generator    <- isJust <$> lookupBind "generator"
      restat       <- isJust <$> lookupBind "restat"
      responseFile <- let (rsp, rspcontent) = ("rspfile", "rspfile_content")
                      in ((,) <$> lookupBind rsp <*> lookupBind rspcontent)
                         >>= (\(ma, mb) -> pure ((,) <$> ma <*> mb))
                         >>= fmap evaluateResponseFile .> sequenceA

      pure (makeRule name command
            |> ruleDescription  .~ description
            |> rulePool         .~ pool
            |> ruleDepfile      .~ depfile
            |> ruleSpecialDeps  .~ specialDeps
            |> ruleGenerator    .~ generator
            |> ruleRestat       .~ restat
            |> ruleResponseFile .~ responseFile)

    evaluateSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe SpecialDeps)
    evaluateSpecialDeps = go
      where
        go (Nothing,           _) = pure Nothing
        go (Just "gcc",        _) = pure (Just makeSpecialDepsGCC)
        go (Just "msvc", mprefix) = pure (Just (makeSpecialDepsMSVC mprefix))
        go (Just owise,        _) = throwUnknownDeps owise

        throwUnknownDeps :: Text -> m a
        throwUnknownDeps deps = ["Unknown `deps` value: ", deps]
                                |> mconcat |> throwEvaluationError

    evaluateResponseFile :: (Text, Text) -> m ResponseFile
    evaluateResponseFile (file, content)
      = pure (makeResponseFile (makePath file) content)

    evaluateTarget :: Text -> m Target
    evaluateTarget = makeTarget .> pure

    evaluateOutput :: Text -> m Output
    evaluateOutput name = do
      target <- evaluateTarget name
      pure (makeOutput target ExplicitOutput)

    evaluateDependency :: (Text, DependencyType) -> m Dependency
    evaluateDependency (name, ty) = do
      target <- evaluateTarget name
      pure (makeDependency target ty)

    evaluateCommand :: Text -> m Command
    evaluateCommand = makeCommand .> pure

    lookupRule :: PBuild -> m (Text, PRule)
    lookupRule pbuild = do
      let name = pbuild ^. pbuildRule
      let throwRuleNotFound = ["Rule not found: ", name]
                              |> mconcat |> throwEvaluationError
      prule <- maybe throwRuleNotFound pure (HM.lookup name prules)
      pure (name, prule)

    computeRuleEnv :: (HashSet Text, PBuild)
                   -> PRule
                   -> Env Text Text
    computeRuleEnv (outs, pbuild) prule = do
      let deps = pbuild ^. pbuildDeps . pdepsNormal

      let composeList :: [a -> a] -> (a -> a)
          composeList = map Endo .> mconcat .> appEndo

          quote :: Text -> Text
          quote x | T.any isSpace x = mconcat ["\"", x, "\""]
          quote x                   = x

      -- the order of adding new environment variables matters
      scopeEnv (pbuild ^. pbuildEnv)
        |> addEnv "out"        (T.unwords (map quote (HS.toList outs)))
        |> addEnv "in"         (T.unwords (map quote (HS.toList deps)))
        |> addEnv "in_newline" (T.unlines (HS.toList deps))
        |> composeList (map (uncurry addEnv) (HM.toList (pbuild ^. pbuildBind)))
        |> addBinds (HM.toList (prule ^. pruleBind))

    prules     :: HashMap Text PRule
    psingles   :: HashMap FileText PBuild
    pmultiples :: HashMap (HashSet FileText) PBuild
    pphonys    :: HashMap Text (HashSet FileText)
    pdefaults  :: HashSet FileText
    ppools     :: HashMap Text Int
    prules     = pninja ^. pninjaRules
    psingles   = pninja ^. pninjaSingles
    pmultiples = pninja ^. pninjaMultiples
    pphonys    = pninja ^. pninjaPhonys
    pdefaults  = pninja ^. pninjaDefaults
    ppools     = pninja ^. pninjaPools

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
