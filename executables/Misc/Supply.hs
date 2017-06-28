-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/Misc/Supply.hs
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
--   Module      : Misc.Supply
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Misc.Supply
  ( -- * @USupply@
    USupply, newUSupply, freshUSupply, splitUSupply

    -- * @SupplyT@
  , SupplyT, SupplyM, runSupply, runSupplyIO
  , fresh, split, split'
  ) where

import           Control.Arrow              ((***), (>>>))
import           Control.Concurrent.Supply
import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Flow

--------------------------------------------------------------------------------

-- | FIXME: doc
data USupply u
  = MkUSupply
    {-# UNPACK #-} !(Int -> u)
    {-# UNPACK #-} !Supply

-- | FIXME: doc
newUSupply :: (Int -> u) -> IO (USupply u)
newUSupply conv = do
  supply <- newSupply
  pure $ MkUSupply conv supply

-- | FIXME: doc
freshUSupply :: USupply u -> (u, USupply u)
freshUSupply (MkUSupply conv supply) = (conv *** MkUSupply conv)
                                       $ freshId supply

-- | FIXME: doc
splitUSupply :: USupply u -> (USupply u, USupply u)
splitUSupply (MkUSupply conv supply) = let (sA, sB) = splitSupply supply
                                       in (MkUSupply conv sA, MkUSupply conv sB)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype SupplyT u m a
  = SupplyT (StateT (USupply u) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | FIXME: doc
type SupplyM u a = SupplyT u Identity a

-- | FIXME: doc
runSupply :: (Monad m) => SupplyT u m a -> USupply u -> m a
runSupply (SupplyT m) = evalStateT m

-- | FIXME: doc
runSupplyIO :: (MonadIO m) => SupplyT u m a -> (Int -> u) -> m a
runSupplyIO m conv = liftIO (newUSupply conv) >>= runSupply m

-- | FIXME: doc
fresh :: (Monad m) => SupplyT u m u
fresh = SupplyT $ StateT (freshUSupply .> pure)

-- | FIXME: doc
split :: (Monad m)
      => SupplyT u m a
      -> SupplyT u m b
      -> SupplyT u m (a, b)
split = split' (\x y -> (,) <$> x <*> y)

-- | FIXME: doc
split' :: (Monad m, Monad n)
       => (m a -> m b -> n (a, b))
       -> SupplyT u m a
       -> SupplyT u m b
       -> SupplyT u n (a, b)
split' combine mA mB = do
  (sA, sB) <- splitUSupply <$> SupplyT get
  lift $ combine (runSupply mA sA) (runSupply mB sB)

--------------------------------------------------------------------------------
