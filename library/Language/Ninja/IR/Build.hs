-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/IR/Build.hs
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
--   Module      : Language.Ninja.IR.Build
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Ninja @build@ declarations.
module Language.Ninja.IR.Build
  ( -- * @Build@
    Build, makeBuild, buildRule, buildOuts, buildDeps
  , BuildConstraint
  ) where

import           Data.Aeson
                 (FromJSON, KeyValue (..), ToJSON, (.:))
import qualified Data.Aeson               as Aeson

import           Data.Text                (Text)

import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

import           Control.DeepSeq          (NFData)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)
import qualified Test.SmallCheck.Series   as SC

import           GHC.Exts                 (Constraint)

import           Language.Ninja.IR.Rule   (Rule)
import           Language.Ninja.IR.Target (Dependency, Output)

import           Control.Lens.Lens        (Lens', lens)

import           Flow                     ((|>))

--------------------------------------------------------------------------------

-- | A Ninja @build@ declaration, as documented
--   <https://ninja-build.org/manual.html#_build_statements here>.
data Build
  = MkBuild
    { _buildRule :: !Rule
    , _buildOuts :: !(HashSet Output)
    , _buildDeps :: !(HashSet Dependency)
    }
  deriving (Eq, Show, Generic)

-- | Construct a default 'Build' from the given 'Rule'
{-# INLINE makeBuild #-}
makeBuild :: Rule -> Build
makeBuild rule = MkBuild
                 { _buildRule = rule
                 , _buildOuts = HS.empty
                 , _buildDeps = HS.empty
                 }

-- | The rule to execute when building any of the outputs.
{-# INLINE buildRule #-}
buildRule :: Lens' Build Rule
buildRule = lens _buildRule
            $ \(MkBuild {..}) x -> MkBuild { _buildRule = x, .. }

-- | The outputs that are built as a result of rule execution.
{-# INLINE buildOuts #-}
buildOuts :: Lens' Build (HashSet Output)
buildOuts = lens _buildOuts
            $ \(MkBuild {..}) x -> MkBuild { _buildOuts = x, .. }

-- | The dependencies that must be satisfied before this can be built.
{-# INLINE buildDeps #-}
buildDeps :: Lens' Build (HashSet Dependency)
buildDeps = lens _buildDeps
            $ \(MkBuild {..}) x -> MkBuild { _buildDeps = x, .. }

-- | Converts to @{rule: …, outputs: …, dependencies: …}@.
instance ToJSON Build where
  toJSON (MkBuild {..})
    = [ "rule"         .= _buildRule
      , "outputs"      .= _buildOuts
      , "dependencies" .= _buildDeps
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Build where
  parseJSON = (Aeson.withObject "Build" $ \o -> do
                  _buildRule <- (o .: "rule")         >>= pure
                  _buildOuts <- (o .: "outputs")      >>= pure
                  _buildDeps <- (o .: "dependencies") >>= pure
                  pure (MkBuild {..}))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Build

-- | Default 'NFData' instance via 'Generic'.
instance NFData Build

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, BuildConstraint (SC.Serial m)) => SC.Serial m Build

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, BuildConstraint (SC.CoSerial m)) => SC.CoSerial m Build

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Build'.
type BuildConstraint (c :: * -> Constraint)
  = ( c Text
    , c (HashSet Output)
    , c (HashSet Dependency)
    )

--------------------------------------------------------------------------------
