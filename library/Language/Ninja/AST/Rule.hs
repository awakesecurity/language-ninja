-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Rule.hs
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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Rule
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
--
--   @since 0.1.0
module Language.Ninja.AST.Rule
  ( Rule, makeRule
  , ruleBind
  , RuleConstraint
  ) where

import qualified Control.Lens              as Lens

import           Flow                      ((|>))

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.Text                 (Text)

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           GHC.Exts                  (Constraint)

import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson                as Aeson

import qualified Language.Ninja.AST.Expr   as AST
import qualified Language.Ninja.Misc       as Misc

--------------------------------------------------------------------------------

-- | A parsed Ninja @rule@ declaration.
--
--   @since 0.1.0
data Rule ann
  = MkRule
    { _ruleAnn  :: !ann
    , _ruleBind :: !(HashMap Text (AST.Expr ann))
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Construct a 'Rule' with all default values
--
--   @since 0.1.0
{-# INLINE makeRule #-}
makeRule :: (Monoid ann) => Rule ann
makeRule = MkRule
           { _ruleAnn  = mempty
           , _ruleBind = mempty
           }

-- | The set of bindings in scope during the execution of this rule.
--
--   @since 0.1.0
{-# INLINE ruleBind #-}
ruleBind :: Lens.Lens' (Rule ann) (HashMap Text (AST.Expr ann))
ruleBind = Lens.lens _ruleBind
           $ \(MkRule {..}) x -> MkRule { _ruleBind = x, .. }

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated Rule where
  annotation' f = Lens.lens _ruleAnn
                  $ \(MkRule {..}) x ->
                      MkRule { _ruleAnn = x
                             , _ruleBind = HM.map (fmap f) _ruleBind
                             , .. }

-- | Converts to @{ann: …, bind: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (Rule ann) where
  toJSON (MkRule {..})
    = [ "ann"  .= _ruleAnn
      , "bind" .= _ruleBind
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (Rule ann) where
  parseJSON = (Aeson.withObject "Rule" $ \o -> do
                  _ruleAnn  <- (o .: "ann")  >>= pure
                  _ruleBind <- (o .: "bind") >>= pure
                  pure (MkRule {..}))

-- | Reasonable 'QC.Arbitrary' instance for 'Rule'.
--
--   @since 0.1.0
instance (QC.Arbitrary ann) => QC.Arbitrary (Rule ann) where
  arbitrary = MkRule <$> QC.arbitrary <*> QC.arbitrary

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (Rule ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (Rule ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, RuleConstraint (SC.Serial m) ann
         ) => SC.Serial m (Rule ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, RuleConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (Rule ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Rule'.
--
--   @since 0.1.0
type RuleConstraint (c :: * -> Constraint) (ann :: *)
  = ( c (HashMap Text (AST.Expr ann))
    , c ann
    )

--------------------------------------------------------------------------------
