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
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
--   Module      : Language.Ninja.Eval
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   Evaluator for the Ninja build language.
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Control.Arrow

import           Language.Ninja.Eval.Pool
import           Language.Ninja.Eval.Rule
import           Language.Ninja.Eval.Target
import           Language.Ninja.Misc.Command
import           Language.Ninja.Misc.IText
import           Language.Ninja.Misc.Path

import           Language.Ninja.Types        (FileStr, Str)
import qualified Language.Ninja.Types        as Ninja

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS (unlines, unwords)

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS

import           Data.Aeson                  as Aeson
import qualified Data.Aeson.Types            as Aeson

import qualified Data.Versions               as V

import qualified Text.Megaparsec             as Mega

import           Data.Data                   (Data)
import           Data.Hashable               (Hashable (..))
import           Data.String                 (IsString (..))
import           GHC.Generics                (Generic)

import           Flow

--------------------------------------------------------------------------------

-- | A parsed and normalized Ninja file.
data Ninja
  = MkNinja
    { _ninjaMeta     :: !Meta
      -- ^ Metadata, which includes top-level variables like @builddir@.
    , _ninjaBuilds   :: !(HashSet Build)
      -- ^ Evaluated @build@ declarations.
    , _ninjaPhonys   :: !(HashMap Target (HashSet Target))
      -- ^ Phony targets, as documented
      --   <https://ninja-build.org/manual.html#_more_details here>.
    , _ninjaDefaults :: !(HashSet Target)
      -- ^ The set of default targets, as documented
      --   <https://ninja-build.org/manual.html#_default_target_statements here>.
    , _ninjaPools    :: !(HashSet Pool)
      -- ^ The set of pools for this
    }
  deriving (Eq, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Ninja

-- | Converts to @{meta: …, builds: …, phonys: …, defaults: …, pools: …}@.
instance ToJSON Ninja where
  toJSON (MkNinja {..})
    = [ "meta"     .= _ninjaMeta
      , "builds"   .= _ninjaBuilds
      , "phonys"   .= _ninjaPhonys
      , "defaults" .= _ninjaDefaults
      , "pools"    .= _ninjaPools
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Ninja where
  parseJSON = (withObject "Ninja" $ \o -> do
                  _ninjaMeta     <- (o .: "meta")     >>= pure
                  _ninjaBuilds   <- (o .: "builds")   >>= pure
                  _ninjaPhonys   <- (o .: "phonys")   >>= pure
                  _ninjaDefaults <- (o .: "defaults") >>= pure
                  _ninjaPools    <- (o .: "pools")    >>= pure
                  pure (MkNinja {..}))

--------------------------------------------------------------------------------

-- | A Ninja @build@ declaration, as documented
--   <https://ninja-build.org/manual.html#_build_statements here>.
data Build
  = MkBuild
    { _buildRule :: !Rule
      -- ^ The rule to execute when building any of the outputs.
    , _buildOuts :: !(HashSet Output)
      -- ^ The outputs that are built as a result of rule execution.
    , _buildDeps :: !(HashSet Dependency)
      -- ^ The dependencies that must be satisfied before this can be built.
    }
  deriving (Eq, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Build

-- | Converts to @{rule: …, outputs: …, dependencies: …}@.
instance ToJSON Build where
  toJSON (MkBuild {..})
    = [ "rule"         .= _buildRule
      , "outputs"      .= _buildOuts
      , "dependencies" .= _buildDeps
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Build where
  parseJSON = (withObject "Build" $ \o -> do
                  _buildRule <- (o .: "rule")         >>= pure
                  _buildOuts <- (o .: "outputs")      >>= pure
                  _buildDeps <- (o .: "dependencies") >>= pure
                  pure (MkBuild {..}))

--------------------------------------------------------------------------------

-- | Ninja top-level metadata, as documented
--   <https://ninja-build.org/manual.html#ref_toplevel here>.
data Meta
  = MkMeta
    { _metaRequiredVersion :: V.SemVer
      -- ^ Corresponds to the @ninja_required_version@ top-level variable.
    , _metaBuildDir        :: Path
      -- ^ Corresponds to the @builddir@ top-level variable.
    }
  deriving (Eq, Ord, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Meta

-- | Converts to @{required-version: …, build-directory: …}@.
instance ToJSON Meta where
  toJSON (MkMeta {..})
    = [ "required-version" .= semverJ _metaRequiredVersion
      , "build-directory"  .= _metaBuildDir
      ] |> object
    where
      semverJ :: V.SemVer -> Value
      semverJ = V.prettySemVer .> toJSON

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Meta where
  parseJSON = (withObject "Meta" $ \o -> do
                  _metaRequiredVersion <- (o .: "required-version") >>= semverP
                  _metaBuildDir        <- (o .: "build-directory")  >>= pure
                  pure (MkMeta {..}))
    where
      semverP :: Value -> Aeson.Parser V.SemVer
      semverP = withText "SemVer" (megaparsecToAeson V.semver')

--------------------------------------------------------------------------------

-- HELPER FUNCTIONS

-- | This function converts a @megaparsec@ parser to an @aeson@ parser.
--   Mainly, it handles converting the error output from @megaparsec@ to a
--   string that is appropriate for 'fail'.
megaparsecToAeson :: Mega.Parsec Mega.Dec Text t
                  -> (Text -> Aeson.Parser t)
megaparsecToAeson parser text = case Mega.runParser parser "" text of
                                  Left  e -> fail (Mega.parseErrorPretty e)
                                  Right x -> pure x

--------------------------------------------------------------------------------
