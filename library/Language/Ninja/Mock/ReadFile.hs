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
--   A typeclass exposing an interface that allows reading a file from a
--   filesystem (virtual or otherwise).
--
--   @since 0.1.0
module Language.Ninja.Mock.ReadFile
  ( MonadReadFile (..)
  ) where

import           Prelude                   hiding (readFile)

import           Control.Monad.Trans.Class (MonadTrans (..))

import qualified Control.Lens              as Lens

import           Data.Text                 (Text)
import qualified Data.Text.IO              as Text

import           Language.Ninja.Misc.Path  (Path, pathString)

import           Flow                      ((.>))

--------------------------------------------------------------------------------

-- Remember: we have imported Prelude hiding 'readFile'.

-- | This typeclass allows you to write code that reads files from the
--   filesystem and then later run that code purely against a virtual filesystem
--   of some description.
--
--   @since 0.1.0
class (Monad m) => MonadReadFile m where
  {-# MINIMAL readFile #-}

  -- | Read the file located at the given path and decode it into 'Text'.
  --
  --   FIXME: some notion of error handling should be encoded into the type
  --
  --   @since 0.1.0
  readFile :: Path -> m Text

-- | The obvious instance for 'IO'.
--
--   @since 0.1.0
instance MonadReadFile IO where
  readFile = Lens.view pathString .> Text.readFile

-- | A placeholder (undecidable) instance that allows this constraint to
--   propagate through monad transformers without needing to 'lift' manually.
--
--   @since 0.1.0
instance ( MonadTrans t, Monad (t m), MonadReadFile m
         ) => MonadReadFile (t m) where
  readFile = readFile .> lift

--------------------------------------------------------------------------------
