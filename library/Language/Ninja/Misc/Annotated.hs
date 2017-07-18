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

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

-- |
--   Module      : Language.Ninja.Misc.Annotated
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A typeclass for AST nodes that are annotated with a polymorphic field,
--   which provides a canonical lens into that field.
--
--   @since 0.1.0
module Language.Ninja.Misc.Annotated
  ( Annotated (..), annotation
  ) where

import qualified Control.Lens as Lens

--------------------------------------------------------------------------------

-- | If you have some type that represents an AST node, it is often useful to
--   add a polymorphic "annotation field" to it, which is used for things like
--   source positions.
--
--   Specifically, suppose we have the following AST node type:
--
--   @data Foo = Foo { _fooBar :: !Bar, _fooBaz :: !Baz } deriving (…)@
--
--   Then an annotation field is added by the following process:
--
--   1. Add an extra (final) type parameter @ann@ to the type.
--   2. Add an extra field @_fooAnn :: !ann@.
--   3. Derive instances of 'Functor', 'Foldable', and 'Traversable'.
--   4. If the type is recursive, add a 'Lens.Plated' instance.
--      See "Language.Ninja.AST.Expr" for a complete example of this.
--   5. Write an 'Annotated' instance with the canonical lens given by the
--      @_fooAnn@ field. There are plenty of examples around this library.
--
--   The end result then looks like:
--
--   > data Foo ann
--   >   = Foo
--   >     { _fooAnn :: !ann
--   >     , _fooBar :: !Bar
--   >     , _fooBaz :: !Baz
--   >     }
--   >   deriving (…, Functor, Foldable, Traversable)
--   >
--   > instance Annotated Foo where
--   >   annotation' = …
--
--   @since 0.1.0
class (Functor ty) => Annotated (ty :: * -> *) where
  -- | Given a function that is used when 'fmap'ing any subterms, return a lens
  --   into the "annotation" field.
  --
  --   When writing an instance, keep in mind that @'annotation'' id@ should
  --   just be the typical definition for a lens into the annotation field.
  --
  --   It should also be true that for any @f :: B -> C@ and @g :: A -> B@,
  --
  --   > annotation' (f . g) == annotation' f . annotation' g
  --
  --   @since 0.1.0
  annotation' :: (ann -> ann') -> Lens.Lens (ty ann) (ty ann') ann ann'

-- | This is just shorthand for @'annotation'' id@.
--
--   @since 0.1.0
annotation :: (Annotated ty) => Lens.Lens' (ty ann) ann
annotation = annotation' id

--------------------------------------------------------------------------------
