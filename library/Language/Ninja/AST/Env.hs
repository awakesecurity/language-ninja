-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Env.hs
--
-- License:
--     Copyright Neil Mitchell  2011-2017.
--     Copyright Awake Security 2017
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions are
--     met:
--
--         * Redistributions of source code must retain the above copyright
--           notice, this list of conditions and the following disclaimer.
--
--         * Redistributions in binary form must reproduce the above
--           copyright notice, this list of conditions and the following
--           disclaimer in the documentation and/or other materials provided
--           with the distribution.
--
--         * Neither the name of Neil Mitchell nor the names of other
--           contributors may be used to endorse or promote products derived
--           from this software without specific prior written permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Env
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   This module contains a type representing a Ninja-style environment along
--   with any supporting or related types.
--
--   @since 0.1.0
module Language.Ninja.AST.Env
  ( Env, makeEnv, fromEnv, headEnv, tailEnv
  , scopeEnv, addEnv, askEnv
  , EnvConstraint
  , Key, Maps
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             ((>=>))

import qualified Control.Lens              as Lens

import           Data.Monoid               (Endo (Endo, appEndo))

import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           GHC.Exts                  (Constraint)

import qualified Data.Aeson                as Aeson

import           Flow                      ((.>), (|>))

--------------------------------------------------------------------------------

-- | A Ninja-style environment, basically a nonempty list of hash tables.
--
--   @since 0.1.0
newtype Env k v
  = MkEnv
    { _fromEnv :: Maps k v
    }
  deriving (Eq, Show, Generic)

-- | Construct an empty environment.
--
--   @since 0.1.0
{-# INLINE makeEnv #-}
makeEnv :: Env k v
makeEnv = MkEnv (HM.empty :| [])

-- | An isomorphism between an 'Env' and a nonempty list of 'HashMap's.
--
--   @since 0.1.0
{-# INLINE fromEnv #-}
fromEnv :: Lens.Iso' (Env k v) (Maps k v)
fromEnv = Lens.iso _fromEnv MkEnv

-- | Get the first 'HashMap' in the underlying nonempty list.
--
--   @since 0.1.0
{-# INLINEABLE headEnv #-}
headEnv :: Env k v -> HashMap k v
headEnv (MkEnv (m :| _)) = m

-- | If the remainder of the underlying nonempty list is nonempty, return
--   the remainder after 'Env' wrapping. Otherwise, return 'Nothing'.
--
--   @since 0.1.0
{-# INLINEABLE tailEnv #-}
tailEnv :: Env k v -> Maybe (Env k v)
tailEnv (MkEnv (_ :| e)) = MkEnv <$> NE.nonEmpty e

-- | Push a new 'Env' onto the stack.
--
--   @since 0.1.0
{-# INLINEABLE scopeEnv #-}
scopeEnv :: Env k v -> Env k v
scopeEnv e = MkEnv (NE.cons HM.empty (_fromEnv e))

-- | Add the given key and value to the given 'Env'.
--
--   @since 0.1.0
{-# INLINEABLE addEnv #-}
addEnv :: (Key k) => k -> v -> Env k v -> Env k v
addEnv k v (MkEnv (m :| rest)) = MkEnv (HM.insert k v m :| rest)

-- | Look up the given key in the given 'Env'.
--
--   @since 0.1.0
{-# INLINEABLE askEnv #-}
askEnv :: (Key k) => Env k v -> k -> Maybe v
askEnv env k = HM.lookup k (headEnv env)
               <|> (tailEnv env >>= (`askEnv` k))

-- | Converts to a (nonempty) array of JSON objects.
--
--   @since 0.1.0
instance (Aeson.ToJSONKey k, Aeson.ToJSON v) => Aeson.ToJSON (Env k v) where
  toJSON = _fromEnv .> NE.toList .> Aeson.toJSON

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance ( Key k, Aeson.FromJSONKey k, Aeson.FromJSON v
         ) => Aeson.FromJSON (Env k v) where
  parseJSON = Aeson.parseJSON
              >=> NE.nonEmpty
              .>  maybe (fail "Env list was empty!") pure
              .>  fmap MkEnv

-- | Reasonable 'QC.Arbitrary' instance for 'Env'.
--
--   @since 0.1.0
instance ( Key k, QC.Arbitrary k, QC.Arbitrary v
         ) => QC.Arbitrary (Env k v) where
  arbitrary = QC.arbitrary
              |> fmap (map (uncurry addEnv .> Endo)
                       .> mconcat
                       .> (\e -> appEndo e makeEnv))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable k, Hashable v) => Hashable (Env k v)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData k, NFData v) => NFData (Env k v)

-- | Uses the underlying 'Maps' instance.
--
--   @since 0.1.0
instance ( Monad m, EnvConstraint (SC.Serial m) k v
         ) => SC.Serial m (Env k v) where
  series = SC.series |> fmap MkEnv

-- | Uses the underlying 'Maps' instance.
--
--   @since 0.1.0
instance ( Monad m, EnvConstraint (SC.CoSerial m) k v
         ) => SC.CoSerial m (Env k v) where
  coseries = SC.coseries .> fmap (\f -> _fromEnv .> f)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'Env'.
--
--   @since 0.1.0
type EnvConstraint (c :: * -> Constraint) (k :: *) (v :: *)
  = (Key k, c k, c v, c (Maps k v))

--------------------------------------------------------------------------------

-- | A constraint alias for @('Eq' k, 'Hashable' k)@.
--
--   @since 0.1.0
type Key k = (Eq k, Hashable k)

-- | A 'NE.NonEmpty' list of 'HashMap's.
--
--   @since 0.1.0
type Maps k v = NE.NonEmpty (HashMap k v)

--------------------------------------------------------------------------------
