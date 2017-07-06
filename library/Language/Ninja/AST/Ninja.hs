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
{-# LANGUAGE DeriveGeneric         #-}
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

import           GHC.Exts                  (Constraint)

import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           Data.Aeson                (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as Aeson

import qualified Language.Ninja.AST.Build  as AST
import qualified Language.Ninja.AST.Rule   as AST

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data Ninja
  = MkNinja
    { _ninjaRules     :: !(HashMap Text AST.Rule)
    , _ninjaSingles   :: !(HashMap Text AST.Build)
    , _ninjaMultiples :: !(HashMap (HashSet Text) AST.Build)
    , _ninjaPhonys    :: !(HashMap Text (HashSet Text))
    , _ninjaDefaults  :: !(HashSet Text)
    , _ninjaPools     :: !(HashMap Text Int)
    , _ninjaSpecials  :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Ninja' with all default values
{-# INLINE makeNinja #-}
makeNinja :: Ninja
makeNinja = MkNinja
            { _ninjaRules     = mempty
            , _ninjaSingles   = mempty
            , _ninjaMultiples = mempty
            , _ninjaPhonys    = mempty
            , _ninjaDefaults  = mempty
            , _ninjaPools     = mempty
            , _ninjaSpecials  = mempty
            }

-- | The rules defined in a parsed Ninja file.
{-# INLINE ninjaRules #-}
ninjaRules :: Lens' Ninja (HashMap Text AST.Rule)
ninjaRules = lens _ninjaRules
             $ \(MkNinja {..}) x -> MkNinja { _ninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
{-# INLINE ninjaSingles #-}
ninjaSingles :: Lens' Ninja (HashMap Text AST.Build)
ninjaSingles = lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens' Ninja (HashMap (HashSet Text) AST.Build)
ninjaMultiples = lens _ninjaMultiples
                 $ \(MkNinja {..}) x -> MkNinja { _ninjaMultiples = x, .. }

-- | The set of phony build declarations.
{-# INLINE ninjaPhonys #-}
ninjaPhonys :: Lens' Ninja (HashMap Text (HashSet Text))
ninjaPhonys = lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets.
{-# INLINE ninjaDefaults #-}
ninjaDefaults :: Lens' Ninja (HashSet Text)
ninjaDefaults = lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
{-# INLINE ninjaPools #-}
ninjaPools :: Lens' Ninja (HashMap Text Int)
ninjaPools = lens _ninjaPools
             $ \(MkNinja {..}) x -> MkNinja { _ninjaPools = x, .. }

-- | A map from "special" top-level variables to their values.
{-# INLINE ninjaSpecials #-}
ninjaSpecials :: Lens' Ninja (HashMap Text Text)
ninjaSpecials = lens _ninjaSpecials
                $ \(MkNinja {..}) x -> MkNinja { _ninjaSpecials = x, .. }

-- | Converts to
--   @{rules: …, singles: …, multiples: …, phonys: …, defaults: …,
--     pools: …, specials: …}@.
instance ToJSON Ninja where
  toJSON (MkNinja {..})
    = [ "rules"     .= _ninjaRules
      , "singles"   .= _ninjaSingles
      , "multiples" .= fixMultiples _ninjaMultiples
      , "phonys"    .= _ninjaPhonys
      , "defaults"  .= _ninjaDefaults
      , "pools"     .= _ninjaPools
      , "specials"  .= _ninjaSpecials
      ] |> Aeson.object
    where
      fixMultiples :: HashMap (HashSet Text) AST.Build -> Aeson.Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> Aeson.toJSON

      printPair :: HashSet Text -> AST.Build -> Aeson.Value
      printPair outputs build =
        Aeson.object ["outputs" .= outputs, "build" .= build]

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Ninja where
  parseJSON = (Aeson.withObject "Ninja" $ \o -> do
                  _ninjaRules     <- (o .: "rules")     >>= pure
                  _ninjaSingles   <- (o .: "singles")   >>= pure
                  _ninjaMultiples <- (o .: "multiples") >>= fixMultiples
                  _ninjaPhonys    <- (o .: "phonys")    >>= pure
                  _ninjaDefaults  <- (o .: "defaults")  >>= pure
                  _ninjaPools     <- (o .: "pools")     >>= pure
                  _ninjaSpecials  <- (o .: "specials")  >>= pure
                  pure (MkNinja {..}))
    where
      fixMultiples :: Aeson.Value
                   -> Aeson.Parser (HashMap (HashSet Text) AST.Build)
      fixMultiples = Aeson.parseJSON
                     >=> mapM parsePair
                     >=> (HM.fromList .> pure)

      parsePair :: Aeson.Value -> Aeson.Parser (HashSet Text, AST.Build)
      parsePair = (Aeson.withObject "Ninja.multiples" $ \o -> do
                      outputs <- (o .: "outputs") >>= pure
                      build   <- (o .: "build")   >>= pure
                      pure (outputs, build))

-- | Reasonable 'QC.Arbitrary' instance for 'Ninja'.
instance QC.Arbitrary Ninja where
  arbitrary = MkNinja
              <$> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Ninja

-- | Default 'NFData' instance via 'Generic'.
instance NFData Ninja

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, NinjaConstraint (SC.Serial m)) => SC.Serial m Ninja

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, NinjaConstraint (SC.CoSerial m)) => SC.CoSerial m Ninja

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Ninja'.
type NinjaConstraint (c :: * -> Constraint)
  = ( AST.BuildConstraint c
    , c (HashMap (HashSet Text) AST.Build)
    , c (HashMap Text (HashSet Text))
    , c (HashMap Text AST.Rule)
    , c (HashMap Text AST.Build)
    , c (HashMap Text Int)
    )

--------------------------------------------------------------------------------
