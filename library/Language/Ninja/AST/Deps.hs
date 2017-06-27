-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Deps.hs
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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Deps
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.AST.Deps
  ( -- * @Deps@
    Deps, makeDeps
  , depsNormal, depsImplicit, depsOrderOnly
  ) where

import           Control.Lens.Lens      (Lens', lens)

import           Flow                   ((|>))

import           Data.HashSet           (HashSet)
import           Data.Text              (Text)

import           Control.DeepSeq        (NFData)
import           Data.Hashable          (Hashable)
import           GHC.Generics           (Generic)

import qualified Test.SmallCheck.Series as SC

import           Data.Aeson             (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson             as Aeson

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
data Deps
  = MkDeps
    { _depsNormal    :: !(HashSet Text)
    , _depsImplicit  :: !(HashSet Text)
    , _depsOrderOnly :: !(HashSet Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Deps' with all default values
{-# INLINE makeDeps #-}
makeDeps :: Deps
makeDeps = MkDeps
            { _depsNormal    = mempty
            , _depsImplicit  = mempty
            , _depsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'Deps'.
{-# INLINE depsNormal #-}
depsNormal :: Lens' Deps (HashSet Text)
depsNormal = lens _depsNormal
             $ \(MkDeps {..}) x -> MkDeps { _depsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'Deps'.
{-# INLINE depsImplicit #-}
depsImplicit :: Lens' Deps (HashSet Text)
depsImplicit = lens _depsImplicit
               $ \(MkDeps {..}) x -> MkDeps { _depsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'Deps'.
{-# INLINE depsOrderOnly #-}
depsOrderOnly :: Lens' Deps (HashSet Text)
depsOrderOnly = lens _depsOrderOnly
                $ \(MkDeps {..}) x -> MkDeps { _depsOrderOnly = x, .. }

-- | Converts to @{normal: …, implicit: …, order-only: …}@.
instance ToJSON Deps where
  toJSON (MkDeps {..})
    = [ "normal"     .= _depsNormal
      , "implicit"   .= _depsImplicit
      , "order-only" .= _depsOrderOnly
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Deps where
  parseJSON = (Aeson.withObject "Deps" $ \o -> do
                  _depsNormal    <- (o .: "normal")     >>= pure
                  _depsImplicit  <- (o .: "implicit")   >>= pure
                  _depsOrderOnly <- (o .: "order-only") >>= pure
                  pure (MkDeps {..}))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Deps

-- | Default 'NFData' instance via 'Generic'.
instance NFData Deps

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, SC.Serial m (HashSet Text)
         ) => SC.Serial m Deps

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, SC.CoSerial m (HashSet Text)
         ) => SC.CoSerial m Deps

--------------------------------------------------------------------------------
