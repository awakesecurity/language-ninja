-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Ninja.hs
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
--   Module      : Language.Ninja.AST.Ninja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
--
--   @since 0.1.0
module Language.Ninja.AST.Ninja
  ( -- * @Ninja@
    Ninja, makeNinja
  , ninjaRules
  , ninjaSingles
  , ninjaMultiples
  , ninjaPhonys
  , ninjaDefaults
  , ninjaPools
  , ninjaSpecials
  , NinjaConstraint
  , Outputs, Output
  ) where

import           Control.Monad             ((>=>))

import qualified Control.Lens              as Lens

import           Flow                      ((.>), (|>))

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.HashSet              (HashSet)

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
import qualified Data.Aeson.Types          as Aeson

import qualified Language.Ninja.AST.Build  as AST
import qualified Language.Ninja.AST.Rule   as AST
import qualified Language.Ninja.Misc       as Misc

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
--
--   @since 0.1.0
data Ninja ann
  = MkNinja
    { _ninjaAnn       :: !ann
    , _ninjaRules     :: !(HashMap Text (AST.Rule ann))
    , _ninjaSingles   :: !(HashMap Output  (AST.Build ann))
    , _ninjaMultiples :: !(HashMap Outputs (AST.Build ann))
    , _ninjaPhonys    :: !(HashMap Text (HashSet Text))
    , _ninjaDefaults  :: !(HashSet Text)
    , _ninjaPools     :: !(HashMap Text Int)
    , _ninjaSpecials  :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Construct a 'Ninja' with all default values
--
--   @since 0.1.0
{-# INLINE makeNinja #-}
makeNinja :: (Monoid ann) => Ninja ann
makeNinja = MkNinja
            { _ninjaAnn       = mempty
            , _ninjaRules     = mempty
            , _ninjaSingles   = mempty
            , _ninjaMultiples = mempty
            , _ninjaPhonys    = mempty
            , _ninjaDefaults  = mempty
            , _ninjaPools     = mempty
            , _ninjaSpecials  = mempty
            }

-- | The rules defined in a parsed Ninja file.
--
--   @since 0.1.0
{-# INLINE ninjaRules #-}
ninjaRules :: Lens.Lens' (Ninja ann) (HashMap Text (AST.Rule ann))
ninjaRules = Lens.lens _ninjaRules
             $ \(MkNinja {..}) x -> MkNinja { _ninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
--
--   @since 0.1.0
{-# INLINE ninjaSingles #-}
ninjaSingles :: Lens.Lens' (Ninja ann) (HashMap Text (AST.Build ann))
ninjaSingles = Lens.lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
--
--   @since 0.1.0
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens.Lens' (Ninja ann) (HashMap Outputs (AST.Build ann))
ninjaMultiples = Lens.lens _ninjaMultiples
                 $ \(MkNinja {..}) x -> MkNinja { _ninjaMultiples = x, .. }

-- | The set of phony build declarations.
--
--   @since 0.1.0
{-# INLINE ninjaPhonys #-}
ninjaPhonys :: Lens.Lens' (Ninja ann) (HashMap Text (HashSet Text))
ninjaPhonys = Lens.lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets.
--
--   @since 0.1.0
{-# INLINE ninjaDefaults #-}
ninjaDefaults :: Lens.Lens' (Ninja ann) (HashSet Text)
ninjaDefaults = Lens.lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
--
--   @since 0.1.0
{-# INLINE ninjaPools #-}
ninjaPools :: Lens.Lens' (Ninja ann) (HashMap Text Int)
ninjaPools = Lens.lens _ninjaPools
             $ \(MkNinja {..}) x -> MkNinja { _ninjaPools = x, .. }

-- | A map from "special" top-level variables to their values.
--
--   @since 0.1.0
{-# INLINE ninjaSpecials #-}
ninjaSpecials :: Lens.Lens' (Ninja ann) (HashMap Text Text)
ninjaSpecials = Lens.lens _ninjaSpecials
                $ \(MkNinja {..}) x -> MkNinja { _ninjaSpecials = x, .. }

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated Ninja where
  annotation' f = Lens.lens _ninjaAnn
                  $ \(MkNinja {..}) x ->
                      MkNinja
                      { _ninjaAnn       = x
                      , _ninjaRules     = HM.map (fmap f) _ninjaRules
                      , _ninjaSingles   = HM.map (fmap f) _ninjaSingles
                      , _ninjaMultiples = HM.map (fmap f) _ninjaMultiples
                      , .. }

-- | Converts to
--   @{ann: …, rules: …, singles: …, multiples: …, phonys: …, defaults: …,
--     pools: …, specials: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (Ninja ann) where
  toJSON (MkNinja {..})
    = [ "ann"       .= _ninjaAnn
      , "rules"     .= _ninjaRules
      , "singles"   .= _ninjaSingles
      , "multiples" .= fixMultiples _ninjaMultiples
      , "phonys"    .= _ninjaPhonys
      , "defaults"  .= _ninjaDefaults
      , "pools"     .= _ninjaPools
      , "specials"  .= _ninjaSpecials
      ] |> Aeson.object
    where
      fixMultiples :: (Aeson.ToJSON ann)
                   => HashMap Outputs (AST.Build ann) -> Aeson.Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> Aeson.toJSON

      printPair :: (Aeson.ToJSON ann) => Outputs -> AST.Build ann -> Aeson.Value
      printPair outputs build
        = Aeson.object ["outputs" .= outputs, "build" .= build]

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (Ninja ann) where
  parseJSON = (Aeson.withObject "Ninja" $ \o -> do
                  _ninjaAnn       <- (o .: "ann")       >>= pure
                  _ninjaRules     <- (o .: "rules")     >>= pure
                  _ninjaSingles   <- (o .: "singles")   >>= pure
                  _ninjaMultiples <- (o .: "multiples") >>= fixMultiples
                  _ninjaPhonys    <- (o .: "phonys")    >>= pure
                  _ninjaDefaults  <- (o .: "defaults")  >>= pure
                  _ninjaPools     <- (o .: "pools")     >>= pure
                  _ninjaSpecials  <- (o .: "specials")  >>= pure
                  pure (MkNinja {..}))
    where
      fixMultiples :: (Aeson.FromJSON ann)
                   => Aeson.Value
                   -> Aeson.Parser (HashMap Outputs (AST.Build ann))
      fixMultiples = Aeson.parseJSON
                     >=> mapM parsePair
                     >=> (HM.fromList .> pure)

      parsePair :: (Aeson.FromJSON ann)
                => Aeson.Value
                -> Aeson.Parser (Outputs, AST.Build ann)
      parsePair = (Aeson.withObject "Ninja.multiples" $ \o -> do
                      outputs <- (o .: "outputs") >>= pure
                      build   <- (o .: "build")   >>= pure
                      pure (outputs, build))

-- | Reasonable 'QC.Arbitrary' instance for 'Ninja'.
--
--   @since 0.1.0
instance (QC.Arbitrary ann) => QC.Arbitrary (Ninja ann) where
  arbitrary = MkNinja
              <$> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (Ninja ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (Ninja ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, NinjaConstraint (SC.Serial m) ann
         ) => SC.Serial m (Ninja ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, NinjaConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (Ninja ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Ninja'.
--
--   @since 0.1.0
type NinjaConstraint (c :: * -> Constraint) (ann :: *)
  = ( AST.BuildConstraint c ann
    , c (HashMap (HashSet Text) (AST.Build ann))
    , c (HashMap Text (HashSet Text))
    , c (HashMap Text (AST.Rule ann))
    , c (HashMap Text (AST.Build ann))
    , c (HashMap Text Int)
    , c ann
    )

-- | A type representing the set of outputs for a build declaration with
--   multiple outputs.
--
--   @since 0.1.0
type Outputs = HashSet Output

-- | A type representing an output of a build declaration.
--
--   @since 0.1.0
type Output = Text

--------------------------------------------------------------------------------
