-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Env.hs
--
-- License:
--     Copyright Neil Mitchell 2011-2017.
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--   Module      : Language.Ninja.Env
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A Ninja-style environment, basically a linked-list of hash tables.
module Language.Ninja.Env
  ( Env, makeEnv, fromEnv, headEnv, tailEnv
  , scopeEnv, addEnv, askEnv
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad       ((>=>))

import           Control.Lens.Iso    (Iso', iso)

import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import           Data.Hashable       (Hashable)
import           GHC.Generics        (Generic)

import           Data.Aeson          (FromJSON(..), FromJSONKey(..), ToJSON(..),
                                      ToJSONKey(..))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types    as Aeson

import           Flow                ((.>))

-- | A Ninja-style environment, basically a linked-list of hash tables.
newtype Env k v
  = MkEnv
    { _fromEnv :: NonEmpty (HashMap k v)
    }
  deriving (Eq, Show, Generic)

-- | Construct an empty environment.
makeEnv :: Env k v
makeEnv = MkEnv (HM.empty :| [])

-- | An isomorphism between an 'Env' and a nonempty list of 'HashMap's.
fromEnv :: Iso' (Env k v) (NonEmpty (HashMap k v))
fromEnv = iso _fromEnv MkEnv

-- | Get the first 'HashMap' in the underlying nonempty list.
headEnv :: Env k v -> HashMap k v
headEnv (MkEnv (m :| _)) = m

-- | If the remainder of the underlying nonempty list is nonempty, return
--   the remainder after 'Env' wrapping. Otherwise, return 'Nothing'.
tailEnv :: Env k v -> Maybe (Env k v)
tailEnv (MkEnv (_ :| e)) = MkEnv <$> NE.nonEmpty e

-- | Push a new 'Env' onto the stack.
scopeEnv :: Env k v -> Env k v
scopeEnv e = MkEnv (NE.cons HM.empty (_fromEnv e))

-- | Add the given key and value to the given 'Env'.
addEnv :: (Eq k, Hashable k) => k -> v -> Env k v -> Env k v
addEnv k v (MkEnv (m :| rest)) = MkEnv (HM.insert k v m :| rest)

-- | Look up the given key in the given 'Env'.
askEnv :: (Eq k, Hashable k) => Env k v -> k -> Maybe v
askEnv env k = HM.lookup k (headEnv env)
               <|> (tailEnv env >>= (`askEnv` k))

-- | Converts to a (non-empty) array of JSON objects.
instance (ToJSONKey k, ToJSON v) => ToJSON (Env k v) where
  toJSON = _fromEnv .> NE.toList .> toJSON

-- | Inverse of the 'ToJSON' instance.
instance (Eq k, Hashable k, FromJSONKey k, FromJSON v) =>
         FromJSON (Env k v) where
  parseJSON = parseJSON
              >=> NE.nonEmpty
              .>  maybe (fail "Env list was empty!") pure
              .>  fmap MkEnv
