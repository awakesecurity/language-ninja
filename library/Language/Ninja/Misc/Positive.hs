-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Positive.hs
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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Misc.Positive
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A data type for integers greater than zero.
--
--   @since 0.1.0
module Language.Ninja.Misc.Positive
  ( Positive, makePositive, fromPositive
  ) where

import           Control.Applicative    (empty)

import           Data.Text              (Text)

import           Data.Aeson             as Aeson

import           Control.DeepSeq        (NFData)
import           Data.Hashable          (Hashable)
import           GHC.Generics           (Generic)
import           Test.SmallCheck        as SC
import           Test.SmallCheck.Series as SC hiding (Positive (..))

import           Control.Lens.Getter

import           Flow

--------------------------------------------------------------------------------

-- | This type represents a positive number; i.e.: an integer greater than zero.
--
--   @since 0.1.0
newtype Positive
  = MkPositive
    { _fromPositive :: Int
    }
  deriving ( Eq, Ord, Real, Integral, Enum, Show, Read, Generic
           , Hashable, NFData
           , ToJSON, FromJSON )

-- | This instance uses 'error' to preserve the 'Positive' invariant.
--
--   @since 0.1.0
instance Num Positive where
  (MkPositive a) + (MkPositive b) = MkPositive (a + b)
  (MkPositive a) * (MkPositive b) = MkPositive (a * b)
  abs = id
  signum = const 1
  negate = ["Prelude.negate: Positive cannot be negated"]
           |> mconcat |> error
  fromInteger i | i > 0     = MkPositive (fromIntegral i)
                | otherwise = [ "Prelude.fromInteger: "
                              , "Positive invariant violated"
                              ] |> mconcat |> error

-- | Constructor for a 'Positive'.
--
--   @since 0.1.0
{-# INLINEABLE makePositive #-}
makePositive :: Int -> Maybe Positive
makePositive i | i > 0     = Just (MkPositive i)
               | otherwise = Nothing

-- | A 'Getter' for the 'Int' underlying a 'Positive'.
--
--   @since 0.1.0
{-# INLINE fromPositive #-}
fromPositive :: Getter Positive Int
fromPositive = to _fromPositive

-- | Uses the underlying 'Int' instance.
--
--   @since 0.1.0
instance (Monad m) => SC.Serial m Positive where
  series = MkPositive <$> (SC.series `suchThat` (> 0))
    where
      suchThat :: SC.Series m a -> (a -> Bool) -> SC.Series m a
      suchThat s p = s >>= \x -> if p x then pure x else empty

-- | Uses the underlying 'Int' instance.
--
--   @since 0.1.0
instance (Monad m) => SC.CoSerial m Positive where
  coseries = coseries .> fmap (\f -> _fromPositive .> f)

--------------------------------------------------------------------------------
