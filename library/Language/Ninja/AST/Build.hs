-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Build.hs
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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.AST.Build
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Ninja @build@ declarations.
module Language.Ninja.AST.Build
  ( -- * @Build@
    Build, makeBuild, buildRule, buildOuts, buildDeps
  ) where

import           Data.Aeson                 (FromJSON,  KeyValue(..), ToJSON,
                                             (.:))
import qualified Data.Aeson                 as Aeson

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Data.Hashable              (Hashable (..))
import           GHC.Generics               (Generic)

import           Language.Ninja.AST.Rule    (Rule)
import           Language.Ninja.AST.Target  (Dependency, Output)

import           Control.Lens.Lens          (Lens')
import qualified Control.Lens

import           Flow                       ((|>))

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
makeBuild :: Rule -> Build
makeBuild rule = MkBuild
                 { _buildRule = rule
                 , _buildOuts = HS.empty
                 , _buildDeps = HS.empty
                 }

-- | The rule to execute when building any of the outputs.
buildRule :: Lens' Build Rule
buildRule = Control.Lens.lens _buildRule
            $ \(MkBuild {..}) x -> MkBuild { _buildRule = x, .. }

-- | The outputs that are built as a result of rule execution.
buildOuts :: Lens' Build (HashSet Output)
buildOuts = Control.Lens.lens _buildOuts
            $ \(MkBuild {..}) x -> MkBuild { _buildOuts = x, .. }

-- | The dependencies that must be satisfied before this can be built.
buildDeps :: Lens' Build (HashSet Dependency)
buildDeps = Control.Lens.lens _buildDeps
            $ \(MkBuild {..}) x -> MkBuild { _buildDeps = x, .. }

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Build

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

--------------------------------------------------------------------------------
