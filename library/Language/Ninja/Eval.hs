-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Eval.hs
--
-- License:
--     Copyright 2017 Awake Networks
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

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Module      : Language.Ninja.Eval
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Control.Arrow

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Except

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS

import           Data.Char
import           Data.Monoid

import           Flow

import           Data.Hashable         (Hashable)
import           GHC.Generics          (Generic)

import qualified Language.Ninja.AST    as NA
import qualified Language.Ninja.Parse  as NP
import qualified Language.Ninja.Types  as NP

--------------------------------------------------------------------------------

debugNinja :: IO NP.PNinja
debugNinja = NP.parse "data/build.ninja"

--------------------------------------------------------------------------------

-- | FIXME: doc
data EvaluationError
  = MkEvaluationError
    { _evalErrorMessage :: !Text
    }
  deriving (Eq, Show, Generic)

instance Exception EvaluationError

throwEvaluationError :: (MonadThrow m) => Text -> m a
throwEvaluationError msg = throwM (MkEvaluationError msg)

--------------------------------------------------------------------------------

evaluate :: forall m. (MonadThrow m) => NP.PNinja -> m NA.Ninja
evaluate pninja = result
  where
    result :: m NA.Ninja
    result = do
      meta     <- metaM
      builds   <- buildsM
      phonys   <- phonysM
      defaults <- defaultsM
      pools    <- poolsM
      pure (NA.makeNinja
            & NA.ninjaMeta     .~ meta
            & NA.ninjaBuilds   .~ builds
            & NA.ninjaPhonys   .~ phonys
            & NA.ninjaDefaults .~ defaults
            & NA.ninjaPools    .~ pools)

    metaM :: m NA.Meta
    metaM = do
      -- FIXME: implement this
      pure NA.makeMeta

    buildsM :: m (HashSet NA.Build)
    buildsM = (pmultiples <> onHM (first HS.singleton) psingles)
              |> HM.toList |> mapM evaluateBuild |> fmap HS.fromList

    phonysM :: m (HashMap NA.Target (HashSet NA.Target))
    phonysM = HM.toList pphonys |> mapM evaluatePhony |> fmap HM.fromList

    defaultsM :: m (HashSet NA.Target)
    defaultsM = HS.toList pdefaults |> mapM evaluateDefault |> fmap HS.fromList

    poolsM :: m (HashSet NA.Pool)
    poolsM = HM.toList ppools |> mapM evaluatePool |> fmap HS.fromList

    evaluateTarget :: NP.Str -> m NA.Target
    evaluateTarget = undefined -- FIXME: implement this

    evaluateBuild :: (HashSet NP.FileStr, NP.PBuild) -> m NA.Build
    evaluateBuild = undefined -- FIXME: implement this

    evaluatePhony :: (NP.Str, HashSet NP.FileStr)
                  -> m (NA.Target, HashSet NA.Target)
    evaluatePhony (name, deps) = do
      ename <- evaluateTarget name
      edeps <- HS.fromList <$> mapM evaluateTarget (HS.toList deps)
      pure (ename, edeps)

    evaluateDefault :: NP.FileStr -> m NA.Target
    evaluateDefault = undefined -- FIXME: implement this

    evaluatePool :: (NP.Str, Int) -> m NA.Pool
    evaluatePool = undefined -- FIXME: implement this

    prules     :: HashMap NP.Str NP.PRule
    psingles   :: HashMap NP.FileStr NP.PBuild
    pmultiples :: HashMap (HashSet NP.FileStr) NP.PBuild
    pphonys    :: HashMap NP.Str (HashSet NP.FileStr)
    pdefaults  :: HashSet NP.FileStr
    ppools     :: HashMap NP.Str Int
    prules     = pninja ^. NP.pninjaRules
    psingles   = pninja ^. NP.pninjaSingles
    pmultiples = pninja ^. NP.pninjaMultiples
    pphonys    = pninja ^. NP.pninjaPhonys
    pdefaults  = pninja ^. NP.pninjaDefaults
    ppools     = pninja ^. NP.pninjaPools

    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

--------------------------------------------------------------------------------
