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

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
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
--
--   @since 0.1.0
module Language.Ninja.AST.Deps
  ( -- * @Deps@
    Deps, makeDeps
  , depsNormal, depsImplicit, depsOrderOnly
  ) where

import qualified Control.Lens              as Lens

import           Flow                      ((|>))

import           Data.HashSet              (HashSet)
import           Data.Text                 (Text)

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson                as Aeson

import qualified Language.Ninja.Misc       as Misc

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
--
--   @since 0.1.0
data Deps ann
  = MkDeps
    { _depsAnn       :: !ann
    , _depsNormal    :: !(HashSet Text)
    , _depsImplicit  :: !(HashSet Text)
    , _depsOrderOnly :: !(HashSet Text)
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Construct a 'Deps' with all default values
--
--   @since 0.1.0
{-# INLINE makeDeps #-}
makeDeps :: (Monoid ann) => Deps ann
makeDeps = MkDeps
            { _depsAnn       = mempty
            , _depsNormal    = mempty
            , _depsImplicit  = mempty
            , _depsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'Deps'.
--
--   @since 0.1.0
{-# INLINE depsNormal #-}
depsNormal :: Lens.Lens' (Deps ann) (HashSet Text)
depsNormal = Lens.lens _depsNormal
             $ \(MkDeps {..}) x -> MkDeps { _depsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'Deps'.
--
--   @since 0.1.0
{-# INLINE depsImplicit #-}
depsImplicit :: Lens.Lens' (Deps ann) (HashSet Text)
depsImplicit = Lens.lens _depsImplicit
               $ \(MkDeps {..}) x -> MkDeps { _depsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'Deps'.
--
--   @since 0.1.0
{-# INLINE depsOrderOnly #-}
depsOrderOnly :: Lens.Lens' (Deps ann) (HashSet Text)
depsOrderOnly = Lens.lens _depsOrderOnly
                $ \(MkDeps {..}) x -> MkDeps { _depsOrderOnly = x, .. }

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated Deps where
  annotation' _ = Lens.lens _depsAnn
                  $ \(MkDeps {..}) x -> MkDeps { _depsAnn = x, .. }

-- | Converts to @{ann: …, normal: …, implicit: …, order-only: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (Deps ann) where
  toJSON (MkDeps {..})
    = [ "ann"        .= _depsAnn
      , "normal"     .= _depsNormal
      , "implicit"   .= _depsImplicit
      , "order-only" .= _depsOrderOnly
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (Deps ann) where
  parseJSON = (Aeson.withObject "Deps" $ \o -> do
                  _depsAnn       <- (o .: "ann")        >>= pure
                  _depsNormal    <- (o .: "normal")     >>= pure
                  _depsImplicit  <- (o .: "implicit")   >>= pure
                  _depsOrderOnly <- (o .: "order-only") >>= pure
                  pure (MkDeps {..}))

-- | Reasonable 'QC.Arbitrary' instance for 'Deps'.
--
--   @since 0.1.0
instance (QC.Arbitrary ann) => QC.Arbitrary (Deps ann) where
  arbitrary = MkDeps
              <$> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (Deps ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (Deps ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, SC.Serial m (HashSet Text), SC.Serial m ann
         ) => SC.Serial m (Deps ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, SC.CoSerial m (HashSet Text), SC.CoSerial m ann
         ) => SC.CoSerial m (Deps ann)

--------------------------------------------------------------------------------
