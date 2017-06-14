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
--   FIXME: doc
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Language.Ninja.Types  (FileStr, Str)
import qualified Language.Ninja.Types  as Ninja

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (unlines, unwords)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS

import           Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson

import qualified Data.Versions         as V

import qualified Text.Megaparsec       as Mega

import           Data.Data             (Data)
import           Data.Hashable         (Hashable)
import           Data.String           (IsString)
import           GHC.Generics          (Generic)

import           Flow

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Path
  = MkPath
    { _pathText :: Text
      -- ^ FIXME: doc
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Target
  = MkTarget
    { _targetText :: Text
      -- ^ FIXME: doc
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype RuleName
  = MkRuleName
    { _ruleNameText :: Text
      -- ^ FIXME: doc
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

--------------------------------------------------------------------------------

-- | FIXME: doc
data PoolName
  = -- | FIXME: doc
    PoolNameDefault
  | -- | FIXME: doc
    PoolNameConsole
  | -- | FIXME: doc
    PoolNameCustom !Text
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME: doc
parsePoolName :: Text -> PoolName
parsePoolName ""        = PoolNameDefault
parsePoolName "console" = PoolNameConsole
parsePoolName t         = PoolNameCustom t

-- | FIXME: doc
printPoolName :: PoolName -> Text
printPoolName PoolNameDefault    = ""
printPoolName PoolNameConsole    = "console"
printPoolName (PoolNameCustom t) = t

instance Hashable PoolName

instance ToJSON PoolName where
  toJSON = printPoolName .> String

instance FromJSON PoolName where
  parseJSON = withText "PoolName" (parsePoolName .> pure)

instance ToJSONKey PoolName where
  toJSONKey = Aeson.toJSONKeyText printPoolName

instance FromJSONKey PoolName where
  fromJSONKey = Aeson.mapFromJSONKeyFunction parsePoolName fromJSONKey

--------------------------------------------------------------------------------

-- | FIXME: doc
data PoolDepth
  = -- | FIXME: doc
    PoolDepth !Int
  | -- | FIXME: doc
    PoolInfinite
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable PoolDepth

instance ToJSON PoolDepth where
  toJSON (PoolDepth i) = toJSON i
  toJSON PoolInfinite  = "infinite"

instance FromJSON PoolDepth where
  parseJSON (v@(Number _))      = PoolDepth <$> parseJSON v
  parseJSON (String "infinite") = pure PoolInfinite
  parseJSON owise               = Aeson.typeMismatch "PoolDepth" owise

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Command
  = MkCommand
    { _commandText :: Text
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Output
  = MkOutput
    { _outputTarget :: !Target
      -- ^ FIXME: doc
    , _outputType   :: !OutputType
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Output

instance ToJSON Output where
  toJSON (MkOutput {..})
    = [ "target" .= _outputTarget
      , "type"   .= _outputType
      ] |> object

instance FromJSON Output where
  parseJSON = (withObject "Output" $ \o -> do
                  _outputTarget <- (o .: "target") >>= pure
                  _outputType   <- (o .: "type")   >>= pure
                  pure (MkOutput {..}))

--------------------------------------------------------------------------------

-- | FIXME: doc
data OutputType
  = -- | FIXME: doc
    ExplicitOutput
  | -- | FIXME: doc
    ImplicitOutput
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable OutputType

instance ToJSON OutputType where
  toJSON ExplicitOutput = "explicit"
  toJSON ImplicitOutput = "implicit"

instance FromJSON OutputType where
  parseJSON = (withText "OutputType" $ \case
                  "explicit" -> pure ExplicitOutput
                  "implicit" -> pure ImplicitOutput
                  owise      -> [ "Invalid output type "
                                , "\"", owise, "\"; should be one of "
                                , "[\"explict\", \"implicit\"]"
                                ] |> mconcat |> T.unpack |> fail)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Dependency
  = MkDependency
    { _dependencyTarget :: !Target
      -- ^ FIXME: doc
    , _dependencyType   :: !DependencyType
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Dependency

instance ToJSON Dependency where
  toJSON (MkDependency {..})
    = [ "target" .= _dependencyTarget
      , "type"   .= _dependencyType
      ] |> object

instance FromJSON Dependency where
  parseJSON = (withObject "Dependency" $ \o -> do
                  _dependencyTarget <- (o .: "target") >>= pure
                  _dependencyType   <- (o .: "type")   >>= pure
                  pure (MkDependency {..}))

--------------------------------------------------------------------------------

-- | FIXME: doc
data DependencyType
  = -- | FIXME: doc
    NormalDependency
  | -- | FIXME: doc
    ImplicitDependency
  | -- | FIXME: doc
    OrderOnlyDependency
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable DependencyType

instance ToJSON DependencyType where
  toJSON NormalDependency    = "normal"
  toJSON ImplicitDependency  = "implicit"
  toJSON OrderOnlyDependency = "order-only"

instance FromJSON DependencyType where
  parseJSON = (withText "DependencyType" $ \case
                  "normal"     -> pure NormalDependency
                  "implicit"   -> pure ImplicitDependency
                  "order-only" -> pure OrderOnlyDependency
                  owise        -> [ "Invalid dependency type "
                                  , "\"", owise, "\"; should be one of "
                                  , "[\"normal\", \"implicit\", \"order-only\"]"
                                  ] |> mconcat |> T.unpack |> fail)

--------------------------------------------------------------------------------

-- | FIXME: doc
data ENinja
  = MkENinja
    { _ninjaMeta     :: !Meta
      -- ^ FIXME: doc
    , _ninjaBuilds   :: !(HashSet EBuild)
      -- ^ FIXME: doc
    , _ninjaPhonys   :: !(HashMap Target (HashSet Target))
      -- ^ FIXME: doc
    , _ninjaDefaults :: !(HashSet Target)
      -- ^ FIXME: doc
    , _ninjaPools    :: !(HashSet EPool)
      -- ^ FIXME: doc
    }
  deriving (Eq, Show, Generic)

instance Hashable ENinja

instance ToJSON ENinja where
  toJSON (MkENinja {..})
    = [ "meta"     .= _ninjaMeta
      , "builds"   .= _ninjaBuilds
      , "phonys"   .= _ninjaPhonys
      , "defaults" .= _ninjaDefaults
      , "pools"    .= _ninjaPools
      ] |> object

instance FromJSON ENinja where
  parseJSON = (withObject "ENinja" $ \o -> do
                  _ninjaMeta     <- (o .: "meta")     >>= pure
                  _ninjaBuilds   <- (o .: "builds")   >>= pure
                  _ninjaPhonys   <- (o .: "phonys")   >>= pure
                  _ninjaDefaults <- (o .: "defaults") >>= pure
                  _ninjaPools    <- (o .: "pools")    >>= pure
                  pure (MkENinja {..}))

--------------------------------------------------------------------------------

-- | FIXME: doc
data ERule
  = MkERule
    { _ruleCommand      :: !Command
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
      --   dependencies.
    , _ruleSpecialDeps  :: !(Maybe SpecialDeps)
      -- ^ If set, enables special dependency processing used in C/C++ header
      --   dependencies. For more information, read the Ninja documentation
      --   <https://ninja-build.org/manual.html#ref_headers here>.
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

instance Hashable ERule

instance ToJSON ERule where
  toJSON (MkERule {..})
    = [ "command"       .= _ruleCommand
      , "description"   .= _ruleDescription
      , "pool"          .= _rulePool
      , "depfile"       .= _ruleDepfile
      , "special-deps"  .= _ruleSpecialDeps
      , "generator"     .= _ruleGenerator
      , "restat"        .= _ruleRestat
      , "response-file" .= _ruleResponseFile
      ] |> object

instance FromJSON ERule where
  parseJSON = (withObject "ERule" $ \o -> do
                  _ruleCommand      <- (o .: "command")       >>= pure
                  _ruleDescription  <- (o .: "description")   >>= pure
                  _rulePool         <- (o .: "pool")          >>= pure
                  _ruleDepfile      <- (o .: "depfile")       >>= pure
                  _ruleSpecialDeps  <- (o .: "special-deps")  >>= pure
                  _ruleGenerator    <- (o .: "generator")     >>= pure
                  _ruleRestat       <- (o .: "restat")        >>= pure
                  _ruleResponseFile <- (o .: "response-file") >>= pure
                  pure (MkERule {..}))

-- | FIXME: doc
makeRule :: Command
         -- ^ The command to run.
         -> ERule
         -- ^ A rule that runs this command.
makeRule cmd = MkERule
               { _ruleCommand      = cmd
               , _ruleDescription  = Nothing
               , _rulePool         = PoolNameDefault
               , _ruleDepfile      = Nothing
               , _ruleSpecialDeps  = Nothing
               , _ruleGenerator    = False
               , _ruleRestat       = False
               , _ruleResponseFile = Nothing
               }

--------------------------------------------------------------------------------

-- | FIXME: doc
data EBuild
  = MkEBuild
    { _buildRule :: !ERule
      -- ^ FIXME: doc
    , _buildOuts :: !(HashSet Output)
      -- ^ FIXME: doc
    , _buildDeps :: !(HashSet Dependency)
      -- ^ FIXME: doc
    }
  deriving (Eq, Show, Generic)

instance Hashable EBuild

instance ToJSON EBuild where
  toJSON (MkEBuild {..})
    = [ "rule"         .= _buildRule
      , "outputs"      .= _buildOuts
      , "dependencies" .= _buildDeps
      ] |> object

instance FromJSON EBuild where
  parseJSON = (withObject "EBuild" $ \o -> do
                  _buildRule <- (o .: "rule")         >>= pure
                  _buildOuts <- (o .: "outputs")      >>= pure
                  _buildDeps <- (o .: "dependencies") >>= pure
                  pure (MkEBuild {..}))

--------------------------------------------------------------------------------

-- | FIXME: doc
data EPool
  = MkEPool
    { _poolName  :: !PoolName
      -- ^ FIXME: doc
    , _poolDepth :: !PoolDepth
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable EPool

instance ToJSON EPool where
  toJSON (MkEPool {..})
    = [ "name"  .= _poolName
      , "depth" .= _poolDepth
      ] |> object

instance FromJSON EPool where
  parseJSON = (withObject "EPool" $ \o -> do
                  _poolName  <- (o .: "name")  >>= pure
                  _poolDepth <- (o .: "depth") >>= pure
                  pure (MkEPool {..}))

--------------------------------------------------------------------------------

-- | FIXME: doc
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_toplevel here>.
data Meta
  = MkMeta
    { _metaRequiredVersion :: V.SemVer
      -- ^ FIXME: doc
    , _metaBuildDir        :: Path
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Meta

instance ToJSON Meta where
  toJSON (MkMeta {..})
    = [ "required-version" .= semverJ _metaRequiredVersion
      , "build-directory"  .= _metaBuildDir
      ] |> object
    where
      semverJ :: V.SemVer -> Value
      semverJ = V.prettySemVer .> toJSON

instance FromJSON Meta where
  parseJSON = (withObject "Meta" $ \o -> do
                  _metaRequiredVersion <- (o .: "required-version") >>= semverP
                  _metaBuildDir        <- (o .: "build-directory")  >>= pure
                  pure (MkMeta {..}))
    where
      semverP :: Value -> Aeson.Parser V.SemVer
      semverP = withText "SemVer" (megaparsecToAeson V.semver')

--------------------------------------------------------------------------------

-- | FIXME: doc
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_headers here>.
data SpecialDeps
  = -- | Special dependency processing for GCC.
    SpecialDepsGCC
  | -- | Special dependency processing for MSVC.
    SpecialDepsMSVC
    { _depsMSVCPrefix :: !(Maybe Text)
      -- ^ This defines the string which should be stripped from @msvc@'s
      --   @/showIncludes@ output. Only needed if the version of Visual Studio
      --   being used is not English.
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable SpecialDeps

instance ToJSON SpecialDeps where
  toJSON = go
    where
      go SpecialDepsGCC             = object ["deps" .= gcc]
      go (SpecialDepsMSVC Nothing)  = object ["deps" .= msvc]
      go (SpecialDepsMSVC (Just p)) = object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Value
      (gcc, msvc) = ("gcc", "msvc")

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

specialDepsGCC :: SpecialDeps
specialDepsGCC = SpecialDepsGCC

specialDepsMSVC :: SpecialDeps
specialDepsMSVC = SpecialDepsMSVC Nothing

--------------------------------------------------------------------------------

-- | FIXME: doc
data ResponseFile
  = MkResponseFile
    { _responseFilePath     :: !Path
      -- ^ FIXME: doc
    , _responseFileContents :: !Text
      -- ^ FIXME: doc
    }
  deriving (Eq, Ord, Show, Generic)

instance Hashable ResponseFile

instance ToJSON ResponseFile where
  toJSON (MkResponseFile {..})
    = [ "path"     .= _responseFilePath
      , "contents" .= _responseFileContents
      ] |> object

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
instance Hashable V.SemVer
instance Hashable V.VUnit

--------------------------------------------------------------------------------

-- HELPER FUNCTIONS

megaparsecToAeson :: Mega.Parsec Mega.Dec Text t
                  -> (Text -> Aeson.Parser t)
megaparsecToAeson parser text = case Mega.runParser parser "" text of
                                  Left  e -> fail (Mega.parseErrorPretty e)
                                  Right x -> pure x

--------------------------------------------------------------------------------
