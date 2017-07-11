-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Mock/ReadFile.hs
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

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--   Module      : Language.Ninja.Mock.ReadFile
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
--
--   @since 0.1.0
module Language.Ninja.Mock.ReadFile
  ( module Language.Ninja.Mock.ReadFile -- FIXME: specific export list
  ) where

import           Prelude                   hiding (readFile)

import           Control.Monad.Trans.Class (MonadTrans (..))

import qualified Control.Lens              as Lens

import           Data.Text                 (Text)
import qualified Data.Text.IO              as Text

import           Language.Ninja.Misc.Path  (Path, makePath, pathString)

import           Flow                      ((.>), (|>))

--------------------------------------------------------------------------------

-- Remember: we have imported Prelude hiding 'readFile'.

-- | FIXME: doc
--
--   @since 0.1.0
class (Monad m) => MonadReadFile m where
  -- | FIXME: doc
  --
  --   @since 0.1.0
  readFile :: Path -> m Text

-- | FIXME: doc
--
--   @since 0.1.0
instance MonadReadFile IO where
  readFile = Lens.view pathString .> Text.readFile

-- | FIXME: doc
--
--   @since 0.1.0
instance ( MonadTrans t, Monad (t m), MonadReadFile m
         ) => MonadReadFile (t m) where
  readFile = readFile .> lift

--------------------------------------------------------------------------------
