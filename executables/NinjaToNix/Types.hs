-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix/Types.hs
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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
--   Module      : NinjaToNix.Types
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Types defined for the @ninja2nix@ executable.
module NinjaToNix.Types
  ( module NinjaToNix.Types -- FIXME: specific export list
  ) where

import           Data.Maybe
import           Data.Monoid

import           Control.Exception
import           Control.Monad.Error.Class
import           Control.Monad.Except

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS

import qualified Language.Ninja.IR         as IR
import qualified Language.Ninja.Misc       as Misc

import           Flow

import           NinjaToNix.Misc.Supply

--------------------------------------------------------------------------------

-- | A simplified build graph.
data SNinja
  = MkSNinja
    { _snBuilds   :: !(HashMap IR.Target SBuild)
    , _snDefaults :: !(HashSet IR.Target)
    }
  deriving (Eq, Show)

-- | Look up the unique build rule that outputs the given target.
lookupBuild :: SNinja -> IR.Target -> Maybe SBuild
lookupBuild (MkSNinja builds _) target = HM.lookup target builds

-- | Compute the set of all targets that are an output of a rule.
allOutputs :: SNinja -> HashSet IR.Target
allOutputs (MkSNinja builds _) = HS.fromList (HM.keys builds)

-- | Compute the set of all targets that are a dependency to a rule.
allInputs :: SNinja -> HashSet IR.Target
allInputs (MkSNinja builds _) = HM.toList builds
                                |> map (snd .> _sbuildDeps)
                                |> mconcat

-- | Compute the set of all targets referenced in the build graph.
allTargets :: SNinja -> HashSet IR.Target
allTargets (sn@(MkSNinja _ defs)) = defs <> allOutputs sn <> allInputs sn

-- | Compute the set of all commands that can be run during a build.
allCommands :: SNinja -> HashSet Misc.Command
allCommands (MkSNinja builds _) = HM.toList builds
                                  |> mapMaybe (snd .> _sbuildCommand)
                                  |> HS.fromList

-- | Compute the set of targets that have no dependencies.
leafTargets :: SNinja -> HashSet IR.Target
leafTargets (sn@(MkSNinja builds _)) = HS.difference (allTargets sn) outputs
  where
    outputs :: HashSet IR.Target
    outputs = HS.fromList $ HM.keys builds

-- | Compute the set of targets that the given target depends on.
targetReferences :: SNinja -> IR.Target -> HashSet IR.Target
targetReferences sninja target = case lookupBuild sninja target of
                                   Just (MkSBuild _ deps) -> deps
                                   Nothing                -> HS.empty

-- | Compute the set of targets that depend on the given target.
targetReferrers :: SNinja -> IR.Target -> HashSet IR.Target
targetReferrers = undefined -- FIXME

--------------------------------------------------------------------------------

data SBuild
  = MkSBuild
    { _sbuildCommand :: Maybe Misc.Command
    , _sbuildDeps    :: HashSet IR.Target
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data SimplifyError
  = UnhandledDepfile      !Misc.Path
  | UnhandledSpecialDeps  !IR.SpecialDeps
  | UnhandledGenerator
  | UnhandledRestat
  | UnhandledResponseFile !IR.ResponseFile
  deriving (Eq, Show)

instance Exception SimplifyError

throwSimplifyError :: (MonadError SimplifyError m) => SimplifyError -> m a
throwSimplifyError = throwError

throwUnhandledDepfile :: (MonadError SimplifyError m) => Misc.Path -> m a
throwUnhandledDepfile path = throwSimplifyError (UnhandledDepfile path)

throwUnhandledSpecialDeps :: (MonadError SimplifyError m)
                          => IR.SpecialDeps -> m a
throwUnhandledSpecialDeps sd = throwSimplifyError (UnhandledSpecialDeps sd)

throwUnhandledGenerator :: (MonadError SimplifyError m) => m a
throwUnhandledGenerator = throwSimplifyError UnhandledGenerator

throwUnhandledRestat :: (MonadError SimplifyError m) => m a
throwUnhandledRestat = throwSimplifyError UnhandledRestat

throwUnhandledResponseFile :: (MonadError SimplifyError m)
                           => IR.ResponseFile -> m a
throwUnhandledResponseFile rsp = throwSimplifyError (UnhandledResponseFile rsp)

--------------------------------------------------------------------------------

newtype SimplifyT m a
  = SimplifyT { fromSimplifyT :: SupplyT IR.Target (ExceptT SimplifyError m) a }
  deriving (Functor, Applicative, Monad, MonadSupply IR.Target)

instance (Monad m) => MonadError SimplifyError (SimplifyT m) where
  throwError = throwError .> SimplifyT
  catchError action handler = SimplifyT (catchError
                                         (fromSimplifyT action)
                                         (handler .> fromSimplifyT))

instance MonadTrans SimplifyT where
  lift = lift .> lift .> SimplifyT

runSimplifyT :: (MonadError SimplifyError m, MonadIO m)
             => SimplifyT m a -> (Int -> IR.Target) -> m a
runSimplifyT (SimplifyT action) convert = do
  out <- runExceptT (execSupplyT action convert)
  case out of
    (Left  e) -> throwError e
    (Right x) -> pure x

--------------------------------------------------------------------------------
