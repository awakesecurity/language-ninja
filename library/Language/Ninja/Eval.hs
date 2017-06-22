-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Eval.hs
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

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Module      : Language.Ninja.Eval
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Control.Applicative        ((<|>))
import           Control.Arrow              (first)

import           Control.Lens.Getter        ((^.))
import           Control.Lens.Setter        ((.~))

import           Control.Exception          (Exception)
import           Control.Monad.Catch        (MonadThrow(..))

import           Data.Char                  (isSpace)
import           Data.Functor               (void)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                ((<>), Endo(..))

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

import           Language.Ninja.AST         (Build, Command, Dependency,
                                             DependencyType(..), Meta, Ninja,
                                             Output, OutputType(..), Pool,
                                             SpecialDeps, ResponseFile, Rule,
                                             Target)
import qualified Language.Ninja.AST         as Ninja
import           Language.Ninja.Env         (askEnv)
import qualified Language.Ninja.Parse       as Ninja
import           Language.Ninja.Types       (Env, FileText, PNinja, PBuild, PRule)
import qualified Language.Ninja.Types       as Ninja

--------------------------------------------------------------------------------

debugNinja :: IO PNinja
debugNinja = Ninja.parse "data/build.ninja"

testEval :: IO ()
testEval = void $ do
  pninja <- debugNinja
  ninja <- evaluate pninja
  LBSC8.putStrLn (Aeson.encodePretty ninja)
  -- print ninja

-------------------------------------------------------------------------------

-- FIXME: split off into Errors.hs

-- | FIXME: doc
data EvalError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalError !Text
  | -- | Errors encountered while computing a 'Meta'.
    EvalMetaError    !EvalMetaError
  | -- | Errors encountered while computing a 'Build'.
    EvalBuildError   !EvalBuildError
  | -- | Errors encountered while computing a 'Rule'.
    EvalRuleError    !EvalRuleError
  | -- | Errors encountered while computing the phony 'HashMap'.
    EvalPhonyError   !EvalPhonyError
  | -- | Errors encountered while computing the default target 'HashSet'.
    EvalDefaultError !EvalDefaultError
  | -- | Errors encountered while computing a 'Pool'.
    EvalPoolError    !EvalPoolError
  deriving (Eq, Show, Generic)

-- | FIXME: doc
instance Exception EvalError

-- | FIXME: doc
throwEvalError :: (MonadThrow m) => EvalError -> m a
throwEvalError = throwM

-- | FIXME: doc
throwGenericEvalError :: (MonadThrow m) => Text -> m a
throwGenericEvalError msg = throwM (GenericEvalError msg)

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalMetaError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalMetaError !Text
  | -- | @Failed to parse `ninja_required_version`: …@
    VersionParseError    !Ver.ParsingError
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalMetaError :: (MonadThrow m) => EvalMetaError -> m a
throwEvalMetaError = EvalMetaError .> throwM

-- | FIXME: doc
throwGenericEvalMetaError :: (MonadThrow m) => Text -> m a
throwGenericEvalMetaError = GenericEvalMetaError .> throwEvalMetaError

-- | FIXME: doc
throwVersionParseError :: (MonadThrow m) => Ver.ParsingError -> m a
throwVersionParseError pe = throwEvalMetaError (VersionParseError pe)

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalPhonyError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalPhonyError !Text
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalPhonyError :: (MonadThrow m) => EvalPhonyError -> m a
throwEvalPhonyError = EvalPhonyError .> throwM

-- | FIXME: doc
throwGenericEvalPhonyError :: (MonadThrow m) => Text -> m a
throwGenericEvalPhonyError = GenericEvalPhonyError .> throwEvalPhonyError

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalDefaultError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalDefaultError !Text
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalDefaultError :: (MonadThrow m) => EvalDefaultError -> m a
throwEvalDefaultError = EvalDefaultError .> throwM

-- | FIXME: doc
throwGenericEvalDefaultError :: (MonadThrow m) => Text -> m a
throwGenericEvalDefaultError = GenericEvalDefaultError .> throwEvalDefaultError

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalBuildError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalBuildError !Text
  | -- | @Rule not found: <text>@
    BuildRuleNotFound     !Text
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalBuildError :: (MonadThrow m) => EvalBuildError -> m a
throwEvalBuildError = EvalBuildError .> throwM

-- | FIXME: doc
throwGenericEvalBuildError :: (MonadThrow m) => Text -> m a
throwGenericEvalBuildError = GenericEvalBuildError .> throwEvalBuildError

-- | FIXME: doc
throwBuildRuleNotFound :: (MonadThrow m) => Text -> m a
throwBuildRuleNotFound name = throwEvalBuildError (BuildRuleNotFound name)

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalRuleError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalRuleError !Text
  | -- | @Lookup failed on rule variable: <text>@
    RuleLookupFailure    !Text
  | -- | @Unknown `deps` value: <text>@
    UnknownDepsValue     !Text
  | -- | @Unexpected `msvc_deps_prefix` for `deps = "<text>"`@
    UnexpectedMSVCPrefix !Text
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalRuleError :: (MonadThrow m) => EvalRuleError -> m a
throwEvalRuleError = EvalRuleError .> throwM

-- | FIXME: doc
throwGenericEvalRuleError :: (MonadThrow m) => Text -> m a
throwGenericEvalRuleError = GenericEvalRuleError .> throwEvalRuleError

-- | FIXME: doc
throwLookupError :: (MonadThrow m) => Text -> m a
throwLookupError v = throwEvalRuleError (RuleLookupFailure v)

-- | FIXME: doc
throwUnknownDeps :: (MonadThrow m) => Text -> m a
throwUnknownDeps deps = throwEvalRuleError (UnknownDepsValue deps)

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvalPoolError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericEvalPoolError !Text
  | -- | @Invalid pool depth for console: <int>@
    InvalidPoolDepth     !Int
  | -- | @Pool name is an empty string@
    EmptyPoolName
  deriving (Eq, Show, Generic)

-- | FIXME: doc
throwEvalPoolError :: (MonadThrow m) => EvalPoolError -> m a
throwEvalPoolError = EvalPoolError .> throwM

-- | FIXME: doc
throwGenericEvalPoolError :: (MonadThrow m) => Text -> m a
throwGenericEvalPoolError = GenericEvalPoolError .> throwEvalPoolError

-- | FIXME: doc
throwInvalidPoolDepth :: (MonadThrow m) => Int -> m a
throwInvalidPoolDepth d = throwEvalPoolError (InvalidPoolDepth d)

-- | FIXME: doc
throwEmptyPoolName :: (MonadThrow m) => m a
throwEmptyPoolName = throwEvalPoolError EmptyPoolName

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
          parseVersion = Ver.version .> either throwVersionParseError pure

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
              |> HM.toList |> mapM evaluateBuild |> fmap HS.fromList

    phonysM :: m (HashMap Target (HashSet Target))
    phonysM = HM.toList pphonys |> mapM evaluatePhony |> fmap HM.fromList

    defaultsM :: m (HashSet Target)
    defaultsM = HS.toList pdefaults |> mapM evaluateDefault |> fmap HS.fromList

    poolsM :: m (HashSet Pool)
    poolsM = HM.toList ppools |> mapM evaluatePool |> fmap HS.fromList

    evaluateBuild :: (HashSet FileText, PBuild) -> m Build
    evaluateBuild (outputs, pbuild) = do
      let pdepsNormal    = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsNormal
      let pdepsImplicit  = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsImplicit
      let pdepsOrderOnly = pbuild ^. Ninja.pbuildDeps . Ninja.pdepsOrderOnly
      let normalDeps     = HS.toList pdepsNormal
      let implicitDeps   = HS.toList pdepsImplicit
      let orderOnlyDeps  = HS.toList pdepsOrderOnly

      rule <- evaluateRule (outputs, pbuild)
      outs <- HS.toList outputs |> mapM evaluateOutput |> fmap HS.fromList
      deps <- let evalDep = flip (curry evaluateDependency)
              in (\n i o -> HS.fromList (n <> i <> o))
                 <$> mapM (evalDep NormalDependency)    normalDeps
                 <*> mapM (evalDep ImplicitDependency)  implicitDeps
                 <*> mapM (evalDep OrderOnlyDependency) orderOnlyDeps

      Ninja.makeBuild rule
        |> Ninja.buildOuts .~ outs
        |> Ninja.buildDeps .~ deps
        |> pure

    evaluatePhony :: (Text, HashSet FileText)
                  -> m (Target, HashSet Target)
    evaluatePhony (name, deps) = do
      ename <- evaluateTarget name
      edeps <- HS.fromList <$> mapM evaluateTarget (HS.toList deps)
      pure (ename, edeps)

    evaluateDefault :: FileText -> m Target
    evaluateDefault = evaluateTarget

    evaluatePool :: (Text, Int) -> m Pool
    evaluatePool ("console", 1) = pure Ninja.poolConsole
    evaluatePool ("console", d) = throwInvalidPoolDepth d
    evaluatePool ("",        _) = throwEmptyPoolName
    evaluatePool (name,  depth) = pure (Ninja.poolCustom name depth)

    evaluateRule :: (HashSet FileText, PBuild) -> m Rule
    evaluateRule (outputs, pbuild) = do
      (name, prule) <- lookupRule pbuild

      let orLookupError :: Text -> Maybe a -> m a
          orLookupError var = maybe (throwLookupError var) pure

      let env = computeRuleEnv (outputs, pbuild) prule

      let lookupBind :: Text -> m (Maybe Text)
          lookupBind = askEnv env .> pure

      let lookupBind_ :: Text -> m Text
          lookupBind_ var = lookupBind var >>= orLookupError var

      command      <- lookupBind_ "command" >>= evaluateCommand
      description  <- lookupBind "description"
      pool         <- let buildBind = pbuild ^. Ninja.pbuildBind
                      in  (HM.lookup "pool" buildBind <|> askEnv env "pool")
                          |> fmap Ninja.parsePoolName
                          |> fromMaybe Ninja.poolNameDefault
                          |> pure
      depfile      <- lookupBind "depfile"
                      |> fmap (fmap Ninja.makePath)
      specialDeps  <- let prefix = "msvc_deps_prefix"
                      in ((,) <$> lookupBind "deps" <*> lookupBind prefix)
                         >>= evaluateSpecialDeps
      generator    <- isJust <$> lookupBind "generator"
      restat       <- isJust <$> lookupBind "restat"
      responseFile <- let (rsp, rspcontent) = ("rspfile", "rspfile_content")
                      in ((,) <$> lookupBind rsp <*> lookupBind rspcontent)
                         >>= (\(ma, mb) -> pure ((,) <$> ma <*> mb))
                         >>= fmap evaluateResponseFile .> sequenceA

      Ninja.makeRule name command
        |> Ninja.ruleDescription  .~ description
        |> Ninja.rulePool         .~ pool
        |> Ninja.ruleDepfile      .~ depfile
        |> Ninja.ruleSpecialDeps  .~ specialDeps
        |> Ninja.ruleGenerator    .~ generator
        |> Ninja.ruleRestat       .~ restat
        |> Ninja.ruleResponseFile .~ responseFile
        |> pure

    evaluateSpecialDeps :: (Maybe Text, Maybe Text) -> m (Maybe SpecialDeps)
    evaluateSpecialDeps = go
      where
        go (Nothing,           _) = do
          pure Nothing
        go (Just "gcc",        _) = do
          pure (Just Ninja.makeSpecialDepsGCC)
        go (Just "msvc", mprefix) = do
          pure (Just (Ninja.makeSpecialDepsMSVC mprefix))
        go (Just owise,        _) = do
          throwUnknownDeps owise

    evaluateResponseFile :: (Text, Text) -> m ResponseFile
    evaluateResponseFile (file, content)
      = pure (Ninja.makeResponseFile (Ninja.makePath file)content)

    evaluateTarget :: Text -> m Target
    evaluateTarget = Ninja.makeTarget .> pure

    evaluateOutput :: Text -> m Output
    evaluateOutput name = do
      target <- evaluateTarget name
      pure (Ninja.makeOutput target ExplicitOutput)

    evaluateDependency :: (Text, DependencyType) -> m Dependency
    evaluateDependency (name, ty) = do
      target <- evaluateTarget name
      pure (Ninja.makeDependency target ty)

    evaluateCommand :: Text -> m Command
    evaluateCommand = Ninja.makeCommand .> pure

    lookupRule :: PBuild -> m (Text, PRule)
    lookupRule pbuild = do
      let name = pbuild ^. Ninja.pbuildRule
      prule <- maybe (throwBuildRuleNotFound name) pure (HM.lookup name prules)
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
