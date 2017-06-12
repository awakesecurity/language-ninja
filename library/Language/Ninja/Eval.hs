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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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

import           Data.Hashable         (Hashable)
import           GHC.Generics          (Generic)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Target
  = MkTarget Text
  deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype RuleName
  = MkRuleName Text
  deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

--------------------------------------------------------------------------------

-- | FIXME: doc
data PoolName
  = PoolNameDefault
  | PoolNameConsole
  | PoolNameCustom !Text
  deriving (Eq, Ord, Show, Generic)

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

-- newtype PoolName
--   = MkPoolName Text
--   deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Command
  = MkCommand Text
  deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Output
  = MkOutput
    { _outputTarget :: !Target
    , _outputType   :: !OutputType
    }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | FIXME: doc
data OutputType
  = ExplicitOutput
    -- ^ FIXME: doc
  | ImplicitOutput
    -- ^ FIXME: doc
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | FIXME: doc
data Dependency
  = MkDependency
    { _dependencyTarget :: !Target
      -- ^ FIXME: doc
    , _dependencyType   :: !DependencyType
      -- ^ FIXME: doc
    }

--------------------------------------------------------------------------------

-- | FIXME: doc
data DependencyType
  = NormalDependency
    -- ^ FIXME: doc
  | ImplicitDependency
    -- ^ FIXME: doc
  | OrderOnlyDependency
    -- ^ FIXME: doc
  deriving ()

--------------------------------------------------------------------------------

-- | FIXME: doc
data ENinja
  = MkENinja
    { _ninjaRules    :: !(HashMap RuleName ERule)
      -- ^ FIXME: doc
    , _ninjaBuilds   :: !(HashMap [Target] EBuild)
      -- ^ FIXME: doc
    , _ninjaPhonys   :: !(HashMap Target [Target])
      -- ^ FIXME: doc
    , _ninjaDefaults :: !(HashSet Target)
      -- ^ FIXME: doc
    , _ninjaPools    :: !(HashMap PoolName Int)
      -- ^ FIXME: doc
    }
  deriving ()

instance ToJSON ENinja where
  toJSON (MkENinja {..}) = object
                           [ "rules"    .= rules
                           , "builds"   .= builds
                           , "phonys"   .= phonys
                           , "defaults" .= defaults
                           , "pools"    .= pools
                           ]
    where
      rules, builds, phonys, defaults, pools :: Value
      rules    = toJSON _ninjaRules
      builds   = toJSON $ map buildPair $ HM.toList _ninjaBuilds
      phonys   = toJSON _ninjaPhonys
      defaults = toJSON _ninjaDefaults
      pools    = toJSON _ninjaPools

      buildPair :: ([Target], EBuild) -> Value
      buildPair (outs, build) = object ["outputs" .= outs, "build" .= build]

instance FromJSON ENinja where
  parseJSON = withObject "ENinja" $ \o -> MkENinja
                                          <$> (o .: "rules"    >>= rulesP)
                                          <*> (o .: "builds"   >>= buildsP)
                                          <*> (o .: "phonys"   >>= phonysP)
                                          <*> (o .: "defaults" >>= defaultsP)
                                          <*> (o .: "pools"    >>= poolsP)
    where
      rulesP    :: Value -> Aeson.Parser (HashMap RuleName ERule)
      rulesP    = parseJSON
      buildsP   :: Value -> Aeson.Parser (HashMap [Target] EBuild)
      buildsP   = \v -> HM.fromList <$> (parseJSON v >>= traverse buildPairP)
      phonysP   :: Value -> Aeson.Parser (HashMap Target [Target])
      phonysP   = parseJSON
      defaultsP :: Value -> Aeson.Parser (HashSet Target)
      defaultsP = parseJSON
      poolsP    :: Value -> Aeson.Parser (HashMap PoolName Int)
      poolsP    = parseJSON

      buildPairP :: Value -> Aeson.Parser ([Target], EBuild)
      buildPairP = withObject "ENinja"
                   $ \o -> (,) <$> (o .: "outputs") <*> (o .: "build")

--------------------------------------------------------------------------------

-- | FIXME: doc
data ERule
  = MkERule
    { _ruleCommand      :: !Command
      -- ^ The command that this rule will run.
    , _ruleDepfile      :: !(Maybe FileStr)
      -- ^ If set, this should be a path to an optional Makefile that contains
      --   extra implicit dependencies. This is used to support C/C++ header
      --   dependencies.
    , _ruleDeps         :: !(Maybe SpecialDeps)
      -- ^ If set, enables special dependency processing used in C/C++ header
      --   dependencies. For more information, read the Ninja documentation
      --   <https://ninja-build.org/manual.html#ref_headers here>.
    , _ruleDescription  :: !(Maybe Text)
      -- ^ A short description of the command, used to pretty-print the command
      --   as it's running. The @ninja -v@ flag controls whether to print the
      --   full command or its description; if a command fails, the full command
      --   line will always be printed before the command's output.
    , _ruleGenerator    :: !Bool
      -- ^ If this is true, specifies that this rule is used to re-invoke the
      --   generator program. Files built using generator rules are treated
      --   specially in two ways: firstly, they will not be rebuilt if the
      --   command line changes; and secondly, they are not cleaned by default.
    , _ruleResponseFile :: !(Maybe (FileStr, Str))
      -- ^ If present, Ninja will use a response file for the given command,
      --   i.e. write the selected string to the given file before calling the
      --   command and delete the file after the command is done.
      --
      --   This is particularly useful on Windows OS, where the maximal length
      --   of a command line is limited and response files must be used instead.
    , _rulePool         :: !PoolName
      -- ^ The process pool in which this rule will be executed.
    }
  deriving (Eq)

instance ToJSON ERule where
  toJSON = undefined -- FIXME

instance FromJSON ERule where
  parseJSON = undefined -- FIXME

-- | FIXME: doc
makeRule :: Command
         -- ^ The command to run.
         -> ERule
         -- ^ A rule that runs this command.
makeRule cmd = MkERule
               { _ruleCommand      = cmd
               , _ruleDepfile      = Nothing
               , _ruleDeps         = Nothing
               , _ruleDescription  = Nothing
               , _ruleGenerator    = False
               , _ruleResponseFile = Nothing
               , _rulePool         = PoolNameDefault
               }

--------------------------------------------------------------------------------

-- | FIXME: doc
data EBuild
  = MkEBuild
    { _buildRule :: !RuleName
      -- ^ FIXME: doc
    , _buildDeps :: !(HashSet Dependency)
      -- ^ FIXME: doc
    }
  deriving ()

instance ToJSON EBuild where
  toJSON = undefined -- FIXME

instance FromJSON EBuild where
  parseJSON = undefined -- FIXME

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
  deriving (Eq)

instance ToJSON SpecialDeps where
  toJSON = go
    where
      go SpecialDepsGCC             = object ["deps" .= gcc]
      go (SpecialDepsMSVC Nothing)  = object ["deps" .= msvc]
      go (SpecialDepsMSVC (Just p)) = object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Value
      gcc = "gcc"
      msvc = "msvc"

instance FromJSON SpecialDeps where
  parseJSON = undefined -- FIXME

specialDepsGCC :: SpecialDeps
specialDepsGCC = SpecialDepsGCC

specialDepsMSVC :: SpecialDeps
specialDepsMSVC = SpecialDepsMSVC Nothing

--------------------------------------------------------------------------------
