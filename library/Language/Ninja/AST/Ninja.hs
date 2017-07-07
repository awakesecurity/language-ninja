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
--   Module      : Language.Ninja.AST.Ninja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
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
  ) where

import           Control.Monad             ((>=>))

import           Control.Lens.Lens         (Lens', lens)

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

import           Data.Aeson                (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as Aeson

import qualified Language.Ninja.AST.Build  as AST
import qualified Language.Ninja.AST.Rule   as AST
import qualified Language.Ninja.Misc       as Misc

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data Ninja ann
  = MkNinja
    { _ninjaAnn       :: !ann
    , _ninjaRules     :: !(HashMap Text (AST.Rule ann))
    , _ninjaSingles   :: !(HashMap Text (AST.Build ann))
    , _ninjaMultiples :: !(HashMap (HashSet Text) (AST.Build ann))
    , _ninjaPhonys    :: !(HashMap Text (HashSet Text))
    , _ninjaDefaults  :: !(HashSet Text)
    , _ninjaPools     :: !(HashMap Text Int)
    , _ninjaSpecials  :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Construct a 'Ninja' with all default values
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
{-# INLINE ninjaRules #-}
ninjaRules :: Lens' (Ninja ann) (HashMap Text (AST.Rule ann))
ninjaRules = lens _ninjaRules
             $ \(MkNinja {..}) x -> MkNinja { _ninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
{-# INLINE ninjaSingles #-}
ninjaSingles :: Lens' (Ninja ann) (HashMap Text (AST.Build ann))
ninjaSingles = lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens' (Ninja ann) (HashMap (HashSet Text) (AST.Build ann))
ninjaMultiples = lens _ninjaMultiples
                 $ \(MkNinja {..}) x -> MkNinja { _ninjaMultiples = x, .. }

-- | The set of phony build declarations.
{-# INLINE ninjaPhonys #-}
ninjaPhonys :: Lens' (Ninja ann) (HashMap Text (HashSet Text))
ninjaPhonys = lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets.
{-# INLINE ninjaDefaults #-}
ninjaDefaults :: Lens' (Ninja ann) (HashSet Text)
ninjaDefaults = lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
{-# INLINE ninjaPools #-}
ninjaPools :: Lens' (Ninja ann) (HashMap Text Int)
ninjaPools = lens _ninjaPools
             $ \(MkNinja {..}) x -> MkNinja { _ninjaPools = x, .. }

-- | A map from "special" top-level variables to their values.
{-# INLINE ninjaSpecials #-}
ninjaSpecials :: Lens' (Ninja ann) (HashMap Text Text)
ninjaSpecials = lens _ninjaSpecials
                $ \(MkNinja {..}) x -> MkNinja { _ninjaSpecials = x, .. }

-- | The usual definition for 'Misc.Annotated'.
instance Misc.Annotated Ninja where
  annotation' f = lens _ninjaAnn
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
instance (ToJSON ann) => ToJSON (Ninja ann) where
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
      fixMultiples :: (ToJSON ann)
                   => HashMap (HashSet Text) (AST.Build ann) -> Aeson.Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> Aeson.toJSON

      printPair :: (ToJSON ann) => HashSet Text -> AST.Build ann -> Aeson.Value
      printPair outputs build =
        Aeson.object ["outputs" .= outputs, "build" .= build]

-- | Inverse of the 'ToJSON' instance.
instance (FromJSON ann) => FromJSON (Ninja ann) where
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
      fixMultiples :: (FromJSON ann)
                   => Aeson.Value
                   -> Aeson.Parser (HashMap (HashSet Text) (AST.Build ann))
      fixMultiples = Aeson.parseJSON
                     >=> mapM parsePair
                     >=> (HM.fromList .> pure)

      parsePair :: (FromJSON ann)
                => Aeson.Value
                -> Aeson.Parser (HashSet Text, AST.Build ann)
      parsePair = (Aeson.withObject "Ninja.multiples" $ \o -> do
                      outputs <- (o .: "outputs") >>= pure
                      build   <- (o .: "build")   >>= pure
                      pure (outputs, build))

-- | Reasonable 'QC.Arbitrary' instance for 'Ninja'.
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
instance (Hashable ann) => Hashable (Ninja ann)

-- | Default 'NFData' instance via 'Generic'.
instance (NFData ann) => NFData (Ninja ann)

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, NinjaConstraint (SC.Serial m) ann
         ) => SC.Serial m (Ninja ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, NinjaConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (Ninja ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Ninja'.
type NinjaConstraint (c :: * -> Constraint) (ann :: *)
  = ( AST.BuildConstraint c ann
    , c (HashMap (HashSet Text) (AST.Build ann))
    , c (HashMap Text (HashSet Text))
    , c (HashMap Text (AST.Rule ann))
    , c (HashMap Text (AST.Build ann))
    , c (HashMap Text Int)
    , c ann
    )

--------------------------------------------------------------------------------
