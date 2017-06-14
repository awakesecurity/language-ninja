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
import           Language.Ninja.Eval.Target
import           Language.Ninja.Misc.IText

import           Language.Ninja.Types       (FileStr, Str)
import qualified Language.Ninja.Types       as Ninja

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS (unlines, unwords)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import qualified Data.Versions              as V

import qualified Text.Megaparsec            as Mega

import           Data.Data                  (Data)
import           Data.Hashable              (Hashable (..))
import           Data.String                (IsString (..))
import           GHC.Generics               (Generic)

import           Flow

--------------------------------------------------------------------------------

-- TODO: Split this module up

--------------------------------------------------------------------------------

-- | This type represents a Unix path string.
newtype Path
  = MkPath
    { _pathText :: IText
      -- ^ The underlying 'IText'.
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

--------------------------------------------------------------------------------

-- | This type represents a POSIX @sh@ command line.
newtype Command
  = MkCommand
    { _commandText :: Text
      -- ^ The underlying 'Text'.
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON )

--------------------------------------------------------------------------------

-- | A parsed and normalized Ninja file.
data ENinja
  = MkENinja
    { _ninjaMeta     :: !Meta
      -- ^ Metadata, which includes top-level variables like @builddir@.
    , _ninjaBuilds   :: !(HashSet EBuild)
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
instance Hashable ENinja

-- | Converts to @{meta: …, builds: …, phonys: …, defaults: …, pools: …}@.
instance ToJSON ENinja where
  toJSON (MkENinja {..})
    = [ "meta"     .= _ninjaMeta
      , "builds"   .= _ninjaBuilds
      , "phonys"   .= _ninjaPhonys
      , "defaults" .= _ninjaDefaults
      , "pools"    .= _ninjaPools
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON ENinja where
  parseJSON = (withObject "ENinja" $ \o -> do
                  _ninjaMeta     <- (o .: "meta")     >>= pure
                  _ninjaBuilds   <- (o .: "builds")   >>= pure
                  _ninjaPhonys   <- (o .: "phonys")   >>= pure
                  _ninjaDefaults <- (o .: "defaults") >>= pure
                  _ninjaPools    <- (o .: "pools")    >>= pure
                  pure (MkENinja {..}))

--------------------------------------------------------------------------------

-- | A Ninja @rule@ declaration, as documented
--   <https://ninja-build.org/manual.html#_rules here>.
data ERule
  = MkERule
    { _ruleName         :: !Text
      -- ^ The name of the rule.
    , _ruleCommand      :: !Command
      -- ^ The command that this rule will run.
    , _ruleDescription  :: !(Maybe Text)
      -- ^ A short description of the command, used to pretty-print the command
      --   as it's running. The @ninja -v@ flag controls whether to print the
      --   full command or its description; if a command fails, the full command
      --   line will always be printed before the command's output.
    , _rulePool         :: !PoolName
      -- ^ The process pool in which this rule will be executed.
    , _ruleDepfile      :: !(Maybe Path)
      -- ^ If set, this should be a path to an optional Makefile that contains
      --   extra implicit dependencies. This is used to support C/C++ header
      --   dependencies. For more information, read the Ninja documentation
      --   <https://ninja-build.org/manual.html#_depfile here>.
    , _ruleSpecialDeps  :: !(Maybe SpecialDeps)
      -- ^ If set, enables special dependency processing used in C/C++ header
      --   dependencies. For more information, read the Ninja documentation
      --   <https://ninja-build.org/manual.html#_deps here>.
    , _ruleGenerator    :: !Bool
      -- ^ If this is true, specifies that this rule is used to re-invoke the
      --   generator program. Files built using generator rules are treated
      --   specially in two ways: firstly, they will not be rebuilt if the
      --   command line changes; and secondly, they are not cleaned by default.
    , _ruleRestat       :: !Bool
      -- ^ If true, causes Ninja to re-stat the command's outputs after
      --   execution of the command. Each output whose modification time the
      --   command did not change will be treated as though it had never needed
      --   to be built. This may cause the output's reverse dependencies to be
      --   removed from the list of pending build actions.
    , _ruleResponseFile :: !(Maybe ResponseFile)
      -- ^ If present, Ninja will use a response file for the given command,
      --   i.e. write the selected string to the given file before calling the
      --   command and delete the file after the command is done.
      --
      --   This is particularly useful on Windows OS, where the maximal length
      --   of a command line is limited and response files must be used instead.
    }
  deriving (Eq, Ord, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable ERule

-- | Converts to
--   @{name: …,
--     command: …,
--     desc: …,
--     pool: …,
--     depfile: …,
--     deps: …,
--     generator: …,
--     restat: …,
--     rsp: …}@.
instance ToJSON ERule where
  toJSON (MkERule {..})
    = [ "name"      .= _ruleName
      , "command"   .= _ruleCommand
      , "desc"      .= _ruleDescription
      , "pool"      .= _rulePool
      , "depfile"   .= _ruleDepfile
      , "deps"      .= _ruleSpecialDeps
      , "generator" .= _ruleGenerator
      , "restat"    .= _ruleRestat
      , "rsp"       .= _ruleResponseFile
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON ERule where
  parseJSON = (withObject "ERule" $ \o -> do
                  _ruleName         <- (o .: "name")      >>= pure
                  _ruleCommand      <- (o .: "command")   >>= pure
                  _ruleDescription  <- (o .: "desc")      >>= pure
                  _rulePool         <- (o .: "pool")      >>= pure
                  _ruleDepfile      <- (o .: "depfile")   >>= pure
                  _ruleSpecialDeps  <- (o .: "deps")      >>= pure
                  _ruleGenerator    <- (o .: "generator") >>= pure
                  _ruleRestat       <- (o .: "restat")    >>= pure
                  _ruleResponseFile <- (o .: "rsp")       >>= pure
                  pure (MkERule {..}))

-- | Construct an 'ERule' with the given name and command, with default values
--   for all other attributes (e.g.: 'False', 'Nothing', 'PoolNameDefault').
makeRule :: Text
         -- ^ The rule name.
         -> Command
         -- ^ The command to run.
         -> ERule
         -- ^ A rule that runs this command.
makeRule name cmd = MkERule
                    { _ruleName         = name
                    , _ruleCommand      = cmd
                    , _ruleDescription  = Nothing
                    , _rulePool         = poolNameDefault
                    , _ruleDepfile      = Nothing
                    , _ruleSpecialDeps  = Nothing
                    , _ruleGenerator    = False
                    , _ruleRestat       = False
                    , _ruleResponseFile = Nothing
                    }

--------------------------------------------------------------------------------

-- | A Ninja @build@ declaration, as documented
--   <https://ninja-build.org/manual.html#_build_statements here>.
data EBuild
  = MkEBuild
    { _buildRule :: !ERule
      -- ^ The rule to execute when building any of the outputs.
    , _buildOuts :: !(HashSet Output)
      -- ^ The outputs that are built as a result of rule execution.
    , _buildDeps :: !(HashSet Dependency)
      -- ^ The dependencies that must be satisfied before this can be built.
    }
  deriving (Eq, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable EBuild

-- | Converts to @{rule: …, outputs: …, dependencies: …}@.
instance ToJSON EBuild where
  toJSON (MkEBuild {..})
    = [ "rule"         .= _buildRule
      , "outputs"      .= _buildOuts
      , "dependencies" .= _buildDeps
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON EBuild where
  parseJSON = (withObject "EBuild" $ \o -> do
                  _buildRule <- (o .: "rule")         >>= pure
                  _buildOuts <- (o .: "outputs")      >>= pure
                  _buildDeps <- (o .: "dependencies") >>= pure
                  pure (MkEBuild {..}))

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

-- | Special dependency information, as described
--   <https://ninja-build.org/manual.html#ref_headers here>.
data SpecialDeps
  = -- | Special dependency processing for GCC.
    SpecialDepsGCC
  | -- | Special dependency processing for MSVC.
    SpecialDepsMSVC
    { _specialDepsPrefix :: !(Maybe Text)
      -- ^ This defines the string which should be stripped from @msvc@'s
      --   @/showIncludes@ output. Only needed if the version of Visual Studio
      --   being used is not English.
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable SpecialDeps

-- | Converts to @{deps: "gcc"}@ or @{deps: "msvc", prefix: …}@.
instance ToJSON SpecialDeps where
  toJSON = go
    where
      go SpecialDepsGCC             = object ["deps" .= gcc]
      go (SpecialDepsMSVC Nothing)  = object ["deps" .= msvc]
      go (SpecialDepsMSVC (Just p)) = object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Value
      (gcc, msvc) = ("gcc", "msvc")

-- | Inverse of the 'ToJSON' instance.
instance FromJSON SpecialDeps where
  parseJSON = withObject "SpecialDeps" $ \o -> do
    deps <- o .: "deps"
    prefix <- o .:? "prefix"
    case T.pack deps of
      "gcc"  -> pure SpecialDepsGCC
      "msvc" -> pure (SpecialDepsMSVC prefix)
      owise  -> ["Invalid deps type ", "\"", owise, "\"; "
                , "should be one of [\"gcc\", \"msvc\"]."
                ] |> mconcat |> T.unpack |> fail

-- | Corresponds to @deps = gcc@.
specialDepsGCC :: SpecialDeps
specialDepsGCC = SpecialDepsGCC

-- | Corresponds to @deps = msvc@.
specialDepsMSVC :: SpecialDeps
specialDepsMSVC = SpecialDepsMSVC Nothing

--------------------------------------------------------------------------------

-- | A response file to use during rule execution, as documented
--   <https://ninja-build.org/manual.html#ref_rule here>.
data ResponseFile
  = MkResponseFile
    { _responseFilePath     :: !Path
      -- ^ Corresponds to @rspfile@.
    , _responseFileContents :: !Text
      -- ^ Corresponds to @rspfile_content@.
    }
  deriving (Eq, Ord, Show, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable ResponseFile

-- | Converts to @{path: …, contents: …}@.
instance ToJSON ResponseFile where
  toJSON (MkResponseFile {..})
    = [ "path"     .= _responseFilePath
      , "contents" .= _responseFileContents
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON ResponseFile where
  parseJSON = withObject "ResponseFile" $ \o -> do
    MkResponseFile
      <$> (o .: "path")
      <*> (o .: "contents")

--------------------------------------------------------------------------------

-- ORPHAN INSTANCES

-- https://github.com/fosskers/versions/issues/6

deriving instance Generic V.SemVer
deriving instance Generic V.VUnit

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable V.SemVer

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable V.VUnit

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
