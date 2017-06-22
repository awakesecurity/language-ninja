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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

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
    Pool, makePool, poolDefault, poolConsole, poolCustom
  , poolName, poolDepth

    -- * @PoolName@
  , PoolName, poolNameDefault, poolNameConsole, poolNameCustom
  , _PoolNameDefault, _PoolNameConsole, _PoolNameCustom
  , printPoolName, parsePoolName

    -- * @PoolDepth@
  , PoolDepth (..)
  , _PoolDepth, _PoolInfinite
  ) where

import           Control.Applicative          (empty)

import           Control.Lens.Fold            (preview)
import           Control.Lens.Getter          (Getter, to, (^.))
import           Control.Lens.Prism

import           Data.Aeson
                 (FromJSON (..), FromJSONKey (..), KeyValue (..), ToJSON (..),
                 ToJSONKey (..), Value (..), (.:))
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Types             as Aeson

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Data.Aeson                   as Aeson
import qualified Data.Aeson.Types             as Aeson

import           Data.Hashable                (Hashable (..))
import           Data.String                  (IsString (..))
import           GHC.Generics                 (Generic)
import           Test.SmallCheck.Series       as SC hiding (Positive)

import           Language.Ninja.Misc.Positive

import           Flow                         ((.>), (|>))

--------------------------------------------------------------------------------

-- | A Ninja @pool@ declaration, as documented
--   <https://ninja-build.org/manual.html#ref_pool here>.
data Pool
  = MkPool
    { _poolName  :: !PoolName
    , _poolDepth :: !PoolDepth
    }
  deriving (Eq, Ord, Show, Read, Generic)


-- | Construct a 'Pool', given its name and depth.
makePool :: PoolName -> PoolDepth -> Maybe Pool
makePool PoolNameDefault    PoolInfinite  = Just poolDefault
makePool PoolNameConsole    (PoolDepth 1) = Just poolConsole
makePool (PoolNameCustom t) (PoolDepth d) = if d >= 1
                                            then fromIntegral d
                                                 |> poolCustom t
                                                 |> Just
                                            else Nothing
makePool _                  _             = Nothing
-- FIXME: use MonadThrow instead of Maybe here

-- | The default pool, i.e.: the one whose name is the empty string.
poolDefault :: Pool
poolDefault = MkPool poolNameDefault PoolInfinite

-- | The @console@ pool.
poolConsole :: Pool
poolConsole = MkPool poolNameConsole (PoolDepth 1)

-- | Create a pool with the given name and depth.
poolCustom :: Text     -- ^ The pool name.
           -> Positive -- ^ The pool depth.
           -> Pool
poolCustom name depth = MkPool (poolNameCustom name) (PoolDepth depth)

-- | A 'Getter' that gives the name of a pool.
poolName :: Getter Pool PoolName
poolName = to _poolName

-- | A 'Getter' that gives the depth of a pool.
poolDepth :: Getter Pool PoolDepth
poolDepth = to _poolDepth

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

-- | Uses the underlying instances.
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m Pool where
  series = pure poolDefault
           \/ pure poolConsole
           \/ (let nameSeries = series >>= (\case ""        -> empty
                                                  "console" -> empty
                                                  x         -> pure x)
               in poolCustom <$> nameSeries <~> series)

-- | Uses the underlying instances.
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m Pool where
  coseries = coseries .> fmap (\f -> convert .> f)
    where
      convert :: Pool -> (PoolName, PoolDepth)
      convert pool = (pool ^. poolName, pool ^. poolDepth)

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

-- | A prism for the 'poolNameDefault' constructor.
_PoolNameDefault :: Prism' PoolName ()
_PoolNameDefault = prism' (const poolNameDefault)
                   $ \case PoolNameDefault -> Just ()
                           _               -> Nothing

-- | A prism for the 'poolNameConsole' constructor.
_PoolNameConsole :: Prism' PoolName ()
_PoolNameConsole = prism' (const poolNameConsole)
                   $ \case PoolNameConsole -> Just ()
                           _               -> Nothing

-- | A prism for the 'poolNameCustom' constructor.
_PoolNameCustom :: Prism' PoolName Text
_PoolNameCustom = prism' poolNameCustom
                  $ \case (PoolNameCustom t) -> Just t
                          _                  -> Nothing

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

-- | Uses the underlying 'Text' instance.
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m PoolName where
  series = parsePoolName <$> (pure "" \/ pure "console" \/ SC.series)

-- | Uses the underlying 'Text' instance.
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m PoolName where
  coseries = SC.coseries
             .> fmap (\f -> printPoolName .> f)

--------------------------------------------------------------------------------

-- | The depth of a Ninja pool.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_pool here>.
data PoolDepth
  = PoolDepth !Positive
  | PoolInfinite
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct a finite 'PoolDepth' from an integer, which should be a number
--   greater than or equal to 1.
makePoolDepth :: Positive -> PoolDepth
makePoolDepth = PoolDepth

-- | Construct an infinite 'PoolDepth'. This constructor is needed for the
--   default pool (@pool = ""@), which has an infinite depth.
makePoolInfinite :: PoolDepth
makePoolInfinite = PoolInfinite

-- | A prism for the 'makePoolDepth' constructor.
_PoolDepth :: Prism' PoolDepth Positive
_PoolDepth = prism' makePoolDepth
             $ \case (PoolDepth i) -> Just i
                     _             -> Nothing

-- | A prism for the 'makePoolInfinite' constructor.
_PoolInfinite :: Prism' PoolDepth ()
_PoolInfinite = prism' (const makePoolInfinite)
                $ \case PoolInfinite -> Just ()
                        _            -> Nothing

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

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m) => SC.Serial m PoolDepth where
  series = pure PoolInfinite
           \/ (series |> fmap (SC.getPositive .> PoolDepth))

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m) => SC.CoSerial m PoolDepth where
  coseries = SC.coseries
             .> fmap (\f -> \case (PoolDepth i) -> f (Just i)
                                  PoolInfinite  -> f Nothing)

--------------------------------------------------------------------------------
