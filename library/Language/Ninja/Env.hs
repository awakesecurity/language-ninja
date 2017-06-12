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
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.Env
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   A Ninja-style environment, basically a linked-list of mutable hash tables.
module Language.Ninja.Env
  ( Env, newEnv, scopeEnv, addEnv, askEnv, fromEnv, getEnvStack
  ) where

import           Control.Monad.IO.Class

import           Data.Hashable

import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM

import           Data.IORef

-- FIXME: it seems unlikely to me that this module actually needs to be
-- using 'IORef's, perhaps a refactoring is in order?

-- | FIXME: doc
data Env k v
  = MkEnv (IORef (HashMap k v)) (Maybe (Env k v))

-- FIXME: this instance is not law-abiding
instance Show (Env k v) where
  show _ = "Env"

-- | FIXME: doc
newEnv :: (MonadIO m) => m (Env k v)
newEnv = do
  ref <- liftIO $ newIORef HM.empty
  pure $ MkEnv ref Nothing

-- | FIXME: doc
scopeEnv :: (MonadIO m) => Env k v -> m (Env k v)
scopeEnv e = do
  ref <- liftIO $ newIORef HM.empty
  pure $ MkEnv ref $ Just e

-- | FIXME: doc
addEnv :: (Eq k, Hashable k, MonadIO m) => Env k v -> k -> v -> m ()
addEnv (MkEnv ref _) k v = liftIO $ modifyIORef ref $ HM.insert k v

-- | FIXME: doc
askEnv :: (Eq k, Hashable k, MonadIO m) => Env k v -> k -> m (Maybe v)
askEnv (MkEnv ref e) k = do
  mp <- liftIO $ readIORef ref
  case HM.lookup k mp
    of Just v  -> pure (Just v)
       Nothing -> case e
                  of Just e' -> askEnv e' k
                     Nothing -> pure Nothing

-- | FIXME: doc
fromEnv :: (MonadIO m) => Env k v -> m (HashMap k v)
fromEnv (MkEnv ref _) = liftIO $ readIORef ref

-- | FIXME: doc
getEnvStack :: (MonadIO m) => Env k v -> m [HashMap k v]
getEnvStack = liftIO . go
  where
    go :: Env k v -> IO [HashMap k v]
    go (MkEnv ref Nothing)     = (: []) <$> readIORef ref
    go (MkEnv ref (Just rest)) = (:) <$> readIORef ref <*> go rest
