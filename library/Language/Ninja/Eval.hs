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

import           Language.Ninja.Eval.Build
import           Language.Ninja.Eval.Meta
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

import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Lens
import           Control.Lens.Prism

import           Flow

--------------------------------------------------------------------------------

-- | A parsed and normalized Ninja file.
data Ninja
  = MkNinja
    { _ninjaMeta     :: !Meta
    , _ninjaBuilds   :: !(HashSet Build)
    , _ninjaPhonys   :: !(HashMap Target (HashSet Target))
    , _ninjaDefaults :: !(HashSet Target)
    , _ninjaPools    :: !(HashSet Pool)
    }
  deriving (Eq, Show, Generic)

-- | Construct a default 'Ninja' value.
makeNinja :: Ninja
makeNinja = MkNinja
            { _ninjaMeta     = makeMeta
            , _ninjaBuilds   = HS.empty
            , _ninjaPhonys   = HM.empty
            , _ninjaDefaults = HS.empty
            , _ninjaPools    = HS.empty
            }

-- | Metadata, which includes top-level variables like @builddir@.
ninjaMeta :: Lens' Ninja Meta
ninjaMeta = lens _ninjaMeta
            $ \(MkNinja {..}) x -> MkNinja { _ninjaMeta = x, .. }

-- | Evaluated @build@ declarations.
ninjaBuilds :: Lens' Ninja (HashSet Build)
ninjaBuilds = lens _ninjaBuilds
              $ \(MkNinja {..}) x -> MkNinja { _ninjaBuilds = x, .. }

-- | Phony targets, as documented
--   <https://ninja-build.org/manual.html#_more_details here>.
ninjaPhonys :: Lens' Ninja (HashMap Target (HashSet Target))
ninjaPhonys = lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets, as documented
--   <https://ninja-build.org/manual.html#_default_target_statements here>.
ninjaDefaults :: Lens' Ninja (HashSet Target)
ninjaDefaults = lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | The set of pools for this Ninja file.
ninjaPools :: Lens' Ninja (HashSet Pool)
ninjaPools = lens _ninjaPools
             $ \(MkNinja {..}) x -> MkNinja { _ninjaPools = x, .. }

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
