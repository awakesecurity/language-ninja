-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Ninja.hs
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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.AST.Ninja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for the Ninja build system AST.
module Language.Ninja.AST.Ninja
  ( -- * @Ninja@
    Ninja, makeNinja
  , ninjaMeta, ninjaBuilds, ninjaPhonys, ninjaDefaults, ninjaPools
  ) where

import           Language.Ninja.AST.Build   (Build)
import           Language.Ninja.AST.Meta    (Meta)
import qualified Language.Ninja.AST.Meta    as Ninja
import           Language.Ninja.AST.Pool    (Pool)
import           Language.Ninja.AST.Target  (Target)

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Data.Aeson                 (FromJSON, KeyValue(..), ToJSON,
                                             (.:))
import qualified Data.Aeson                 as Aeson

import           Data.Hashable              (Hashable)
import           GHC.Generics               (Generic)

import           Control.Lens.Lens          (Lens')
import qualified Control.Lens

import           Flow                       ((|>))

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
            { _ninjaMeta     = Ninja.makeMeta
            , _ninjaBuilds   = HS.empty
            , _ninjaPhonys   = HM.empty
            , _ninjaDefaults = HS.empty
            , _ninjaPools    = HS.empty
            }

-- | Metadata, which includes top-level variables like @builddir@.
ninjaMeta :: Lens' Ninja Meta
ninjaMeta = Control.Lens.lens _ninjaMeta
            $ \(MkNinja {..}) x -> MkNinja { _ninjaMeta = x, .. }

-- | Evaluated @build@ declarations.
ninjaBuilds :: Lens' Ninja (HashSet Build)
ninjaBuilds = Control.Lens.lens _ninjaBuilds
              $ \(MkNinja {..}) x -> MkNinja { _ninjaBuilds = x, .. }

-- | Phony targets, as documented
--   <https://ninja-build.org/manual.html#_more_details here>.
ninjaPhonys :: Lens' Ninja (HashMap Target (HashSet Target))
ninjaPhonys = Control.Lens.lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets, as documented
--   <https://ninja-build.org/manual.html#_default_target_statements here>.
ninjaDefaults :: Lens' Ninja (HashSet Target)
ninjaDefaults = Control.Lens.lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | The set of pools for this Ninja file.
ninjaPools :: Lens' Ninja (HashSet Pool)
ninjaPools = Control.Lens.lens _ninjaPools
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
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Ninja where
  parseJSON = (Aeson.withObject "Ninja" $ \o -> do
                  _ninjaMeta     <- (o .: "meta")     >>= pure
                  _ninjaBuilds   <- (o .: "builds")   >>= pure
                  _ninjaPhonys   <- (o .: "phonys")   >>= pure
                  _ninjaDefaults <- (o .: "defaults") >>= pure
                  _ninjaPools    <- (o .: "pools")    >>= pure
                  pure (MkNinja {..}))

--------------------------------------------------------------------------------
