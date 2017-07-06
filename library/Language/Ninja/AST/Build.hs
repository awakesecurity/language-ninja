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
--   Module      : Language.Ninja.AST.Build
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.AST.Build
  ( -- * @Build@
    Build, makeBuild
  , buildRule, buildEnv, buildDeps, buildBind
  , BuildConstraint
  ) where

import qualified Control.Lens              as Lens
import           Control.Lens.Lens         (Lens', lens)

import           Flow                      ((|>))

import           Data.HashMap.Strict       (HashMap)
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

import qualified Language.Ninja.AST.Deps   as AST
import qualified Language.Ninja.AST.Env    as AST

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data Build
  = MkBuild
    { _buildRule :: !Text
    , _buildEnv  :: !(AST.Env Text Text)
    , _buildDeps :: !AST.Deps
    , _buildBind :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Build' with all default values.
{-# INLINE makeBuild #-}
makeBuild :: Text
          -- ^ The rule name
          -> AST.Env Text Text
          -- ^ The environment
          -> Build
makeBuild rule env = MkBuild
                     { _buildRule = rule
                     , _buildEnv  = env
                     , _buildDeps = AST.makeDeps
                     , _buildBind = mempty
                     }

-- | A lens into the rule name associated with a 'Build'.
{-# INLINE buildRule #-}
buildRule :: Lens' Build Text
buildRule = lens _buildRule
            $ \(MkBuild {..}) x -> MkBuild { _buildRule = x, .. }

-- | A lens into the environment associated with a 'Build'.
{-# INLINE buildEnv #-}
buildEnv :: Lens' Build (AST.Env Text Text)
buildEnv = lens _buildEnv
           $ \(MkBuild {..}) x -> MkBuild { _buildEnv = x, .. }

-- | A lens into the dependencies associated with a 'Build'.
{-# INLINE buildDeps #-}
buildDeps :: Lens' Build AST.Deps
buildDeps = lens _buildDeps
            $ \(MkBuild {..}) x -> MkBuild { _buildDeps = x, .. }

-- | A lens into the bindings associated with a 'Build'.
{-# INLINE buildBind #-}
buildBind :: Lens' Build (HashMap Text Text)
buildBind = lens _buildBind
            $ \(MkBuild {..}) x -> MkBuild { _buildBind = x, .. }

-- | Converts to @{rule: …, env: …, deps: …, bind: …}@.
instance ToJSON Build where
  toJSON (MkBuild {..})
    = [ "rule" .= _buildRule
      , "env"  .= _buildEnv
      , "deps" .= _buildDeps
      , "bind" .= _buildBind
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Build where
  parseJSON = (Aeson.withObject "Build" $ \o -> do
                  _buildRule <- (o .: "rule") >>= pure
                  _buildEnv  <- (o .: "env")  >>= pure
                  _buildDeps <- (o .: "deps") >>= pure
                  _buildBind <- (o .: "bind") >>= pure
                  pure (MkBuild {..}))

-- | Reasonable 'QC.Arbitrary' instance for 'Build'.
instance QC.Arbitrary Build where
  arbitrary = MkBuild
              <$> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary
              <*> QC.arbitrary

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
    , c (HashSet Text)
    , c (HashMap Text Text)
    , c (AST.Maps Text Text)
    )

--------------------------------------------------------------------------------
