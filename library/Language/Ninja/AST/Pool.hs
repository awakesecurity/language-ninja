-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Pool.hs
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
--   Module      : Language.Ninja.AST.Pool
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Types relating to Ninja @pool@s.
module Language.Ninja.AST.Pool
  ( -- * @Pool@
    Pool, poolDefault, poolConsole, poolCustom
  , poolName, poolDepth

    -- * @PoolName@
  , PoolName, poolNameDefault, poolNameConsole, poolNameCustom
  , printPoolName, parsePoolName

    -- * @PoolDepth@
  , PoolDepth (..)
  ) where

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Data.Aeson          (FromJSON(..), FromJSONKey(..),
                                      KeyValue(..), ToJSON(..), ToJSONKey(..),
                                      Value(..), (.:))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types    as Aeson

import           Data.Hashable       (Hashable (..))
import           Data.String         (IsString (..))
import           GHC.Generics        (Generic)

import           Control.Lens.Getter (Getter, to)

import           Flow                ((|>), (.>))

--------------------------------------------------------------------------------

-- | A Ninja @pool@ declaration, as documented
--   <https://ninja-build.org/manual.html#ref_pool here>.
data Pool
  = MkPool
    { _poolName  :: !PoolName
    , _poolDepth :: !PoolDepth
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | A 'Getter' that gives the name of a pool.
poolName :: Getter Pool PoolName
poolName = to _poolName

-- | A 'Getter' that gives the depth of a pool.
poolDepth :: Getter Pool PoolDepth
poolDepth = to _poolDepth

-- | The default pool, i.e.: the one whose name is the empty string.
poolDefault :: Pool
poolDefault = MkPool poolNameDefault PoolInfinite

-- | The @console@ pool.
poolConsole :: Pool
poolConsole = MkPool poolNameConsole (PoolDepth 1)

-- | Create a pool with the given name and depth.
poolCustom :: Text -- ^ The pool name.
           -> Int  -- ^ The pool depth.
           -> Pool
poolCustom name depth = MkPool (poolNameCustom name) (PoolDepth depth)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Pool

-- | Converts to @{name: …, depth: …}@.
instance ToJSON Pool where
  toJSON (MkPool {..})
    = [ "name"  .= _poolName
      , "depth" .= _poolDepth
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Pool where
  parseJSON = (Aeson.withObject "Pool" $ \o -> do
                  _poolName  <- (o .: "name")  >>= pure
                  _poolDepth <- (o .: "depth") >>= pure
                  pure (MkPool {..}))

--------------------------------------------------------------------------------

-- | The name of a Ninja pool.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_pool here>.
data PoolName
  = PoolNameDefault
  | PoolNameConsole
  | PoolNameCustom !Text
  deriving (Eq, Ord, Show, Read, Generic)

-- | Create a 'PoolName' corresponding to the built-in default pool, i.e.: the
--   pool that is selected if the @pool@ attribute is set to the empty string.
poolNameDefault :: PoolName
poolNameDefault = PoolNameDefault

-- | Create a 'PoolName' corresponding to the built-in @console@ pool.
poolNameConsole :: PoolName
poolNameConsole = PoolNameConsole

-- | Create a 'PoolName' corresponding to a custom pool.
poolNameCustom :: Text -> PoolName
poolNameCustom ""        = error "Invalid pool name: \"\""
poolNameCustom "console" = error "Invalid pool name: \"console\""
poolNameCustom text      = PoolNameCustom text

-- | Convert a 'PoolName' to the string that, if the @pool@ attribute is set to
--   it, will cause the given 'PoolName' to be parsed.
--
--   >>> printPoolName poolNameDefault
--   ""
--
--   >>> printPoolName poolNameConsole
--   "console"
--
--   >>> printPoolName (poolNameCustom "foobar")
--   "foobar"
printPoolName :: PoolName -> Text
printPoolName PoolNameDefault    = ""
printPoolName PoolNameConsole    = "console"
printPoolName (PoolNameCustom t) = t

-- | Inverse of 'printPoolName'.
--
--   >>> parsePoolName ""
--   PoolNameDefault
--
--   >>> parsePoolName "console"
--   PoolNameConsole
--
--   >>> parsePoolName "foobar"
--   PoolNameCustom "foobar"
parsePoolName :: Text -> PoolName
parsePoolName ""        = poolNameDefault
parsePoolName "console" = poolNameConsole
parsePoolName t         = poolNameCustom t

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable PoolName

-- | Converts from string via 'parsePoolName'.
instance IsString PoolName where
  fromString = T.pack .> parsePoolName

-- | Converts to JSON string via 'printPoolName'.
instance ToJSON PoolName where
  toJSON = printPoolName .> String

-- | Inverse of the 'ToJSON' instance.
instance FromJSON PoolName where
  parseJSON = Aeson.withText "PoolName" (parsePoolName .> pure)

-- | Converts to JSON string via 'printPoolName'.
instance ToJSONKey PoolName where
  toJSONKey = Aeson.toJSONKeyText printPoolName

-- | Inverse of the 'ToJSONKey' instance.
instance FromJSONKey PoolName where
  fromJSONKey = Aeson.mapFromJSONKeyFunction parsePoolName fromJSONKey

--------------------------------------------------------------------------------

-- | The depth of a Ninja pool.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_pool here>.
data PoolDepth
  = -- | Construct a normal pool with the given depth, which should be a
    --   natural number.
    PoolDepth !Int
  | -- | This constructor is needed for the default pool (@pool = ""@), which
    --   has an infinite depth.
    PoolInfinite
  deriving (Eq, Ord, Show, Read, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable PoolDepth

-- | Converts 'PoolInfinite' to @"infinite"@ and 'PoolDepth' to the
--   corresponding JSON number.
instance ToJSON PoolDepth where
  toJSON (PoolDepth i) = toJSON i
  toJSON PoolInfinite  = "infinite"

-- | Inverse of the 'ToJSON' instance.
instance FromJSON PoolDepth where
  parseJSON (v@(Number _))      = PoolDepth <$> parseJSON v
  parseJSON (String "infinite") = pure PoolInfinite
  parseJSON owise               = Aeson.typeMismatch "PoolDepth" owise

--------------------------------------------------------------------------------
