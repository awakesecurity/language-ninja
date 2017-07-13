-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix/Misc/Supply.hs
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

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : NinjaToNix.Misc.Supply
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A monad transformer that exposes a nicer interface to the datatype
--   defined in @concurrent-supply@.
module NinjaToNix.Misc.Supply
  ( -- * @MonadSupply@
    MonadSupply (..)

    -- * @USupply@
  , USupply, newUSupply, freshUSupply, splitUSupply

    -- * @SupplyT@
  , SupplyT, runSupplyT, execSupplyT, splitSupplyT
  ) where

import           Control.Applicative              (Alternative, liftA2)
import           Control.Arrow                    ((***))
import           Control.Monad                    (MonadPlus)
import           Control.Monad.Fail               (MonadFail)
import           Control.Monad.Fix                (MonadFix)

import           Control.Monad.Trans.State.Strict (StateT (..))
import qualified Control.Monad.Trans.State.Strict as StateT

import           Control.Monad.Error.Class        (MonadError (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.State.Class        (MonadState (..))
import           Control.Monad.Trans.Class        (MonadTrans (..))

import qualified Control.Concurrent.Supply        as CS

import           Flow

--------------------------------------------------------------------------------

-- | A monad in which a splittable supply of fresh variables is available.
class (Monad m) => MonadSupply u m | m -> u where
  {-# MINIMAL fresh, split #-}

  -- | Create a fresh value of type @u@.
  fresh :: m u

  -- | Run two actions by splitting the supply into two streams that always
  --   return disjoint values. This is useful for example when doing a recursive
  --   renaming pass on some tree.
  split :: m a -> m b -> m (a, b)

--------------------------------------------------------------------------------

-- | A supply for unique values of a given type. If you can come up with an
--   injective map from 'Int' to your chosen type, then you can make a 'USupply'
--   for that type.
data USupply u
  = MkUSupply
    { _usupplyConversion ::                !(Int -> u)
    , _usupplyUnderlying :: {-# UNPACK #-} !CS.Supply
    }
  deriving (Functor)

-- | Given a injective map from @'Int' -> u@, create a @'USupply' u@.
{-# INLINEABLE newUSupply #-}
newUSupply :: (Int -> u) -> IO (USupply u)
newUSupply conv = do
  supply <- CS.newSupply
  pure $ MkUSupply conv supply

-- | Get a fresh value of type @u@ from a given @'USupply' u@, along with a
--   new @'USupply' u@ from which more elements can be taken.
{-# INLINEABLE freshUSupply #-}
freshUSupply :: USupply u -> (u, USupply u)
freshUSupply (MkUSupply conv supply) = CS.freshId supply
                                       |> conv *** MkUSupply conv

-- | Split a @'USupply' u@ into two streams that yield disjoint values, assuming
--   that the function originally given to 'newUSupply' was injective.
{-# INLINEABLE splitUSupply #-}
splitUSupply :: USupply u -> (USupply u, USupply u)
splitUSupply (MkUSupply conv supply) = let (sA, sB) = CS.splitSupply supply
                                       in (MkUSupply conv sA, MkUSupply conv sB)

--------------------------------------------------------------------------------

-- | A value of type @'SupplyT' u m a@ is merely a newtype wrapper
--   around @'StateT' ('USupply' u) m a@ that prevents arbitrary modification
--   of the underlying 'USupply' (i.e.: it is an abstract data type that
--   enforces the following invariant: any change to the 'USupply' held by the
--   state should be the result of running 'freshUSupply').
newtype SupplyT u m a
  = SupplyT { fromSupplyT :: StateT (USupply u) m a }
  deriving ( Functor, Applicative, Alternative, Monad
           , MonadTrans, MonadPlus, MonadFix, MonadFail )

-- | Given a starting 'USupply', run the given 'SupplyT'.
runSupplyT :: (Monad m) => SupplyT u m a -> USupply u -> m a
runSupplyT (SupplyT m) = StateT.evalStateT m

-- | Given a function @f@ from @'Int' -> u@, run the given 'SupplyT' by creating
--   a 'USupply' from @f@ (which requires IO) and then
execSupplyT :: (MonadIO m) => SupplyT u m a -> (Int -> u) -> m a
execSupplyT m conv = liftIO (newUSupply conv) >>= runSupplyT m

-- | This is a generalized form of 'split' that allows passing in an arbitrary
--   "combining" function. The simplest case of a combining function would be
--   @'liftM2' (,)@, but this might be a place where parallelism is added for
--   the right underlying monad.
splitSupplyT :: (Monad m, Monad n)
             => (m a -> m b -> n (a, b))
             -> SupplyT u m a
             -> SupplyT u m b
             -> SupplyT u n (a, b)
splitSupplyT combine mA mB = do
  (sA, sB) <- splitUSupply <$> SupplyT get
  lift $ combine (runSupplyT mA sA) (runSupplyT mB sB)

instance (MonadError e m) => MonadError e (SupplyT u m) where
  throwError = throwError .> lift
  catchError action handler
    = SupplyT (catchError (fromSupplyT action) (handler .> fromSupplyT))

instance (Monad m) => MonadSupply u (SupplyT u m) where
  fresh = SupplyT $ StateT (freshUSupply .> pure)
  split = splitSupplyT (liftA2 (,))

--------------------------------------------------------------------------------
