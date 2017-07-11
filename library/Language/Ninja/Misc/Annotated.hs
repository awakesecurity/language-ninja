-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Annotated.hs
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

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

-- |
--   Module      : Language.Ninja.Misc.Annotated
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
--
--   @since 0.1.0
module Language.Ninja.Misc.Annotated
  ( Annotated (..), annotation
  ) where

import qualified Control.Lens as Lens

--------------------------------------------------------------------------------

-- | FIXME: doc
--
--   @since 0.1.0
class (Functor ty) => Annotated (ty :: * -> *) where
  -- | FIXME: doc
  --
  --   @since 0.1.0
  annotation' :: (ann -> ann') -> Lens.Lens (ty ann) (ty ann') ann ann'

-- | FIXME: doc
--
--   @since 0.1.0
annotation :: (Annotated ty) => Lens.Lens' (ty ann) ann
annotation = annotation' id

--------------------------------------------------------------------------------
