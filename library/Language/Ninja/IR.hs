-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/IR.hs
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

-- |
--   Module      : Language.Ninja.IR
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The Ninja build language intermediate representation after compilation.
--
--   This module re-exports all of the modules under the "Language.Ninja.IR"
--   namespace for convenience.
--
--   It is recommended that you import it with the following style:
--
--   > import qualified Language.Ninja.IR as IR
--
--   @since 0.1.0
module Language.Ninja.IR
  ( -- * "Language.Ninja.IR.Ninja"
    IR.Ninja, IR.makeNinja
  , IR.ninjaMeta, IR.ninjaBuilds, IR.ninjaPhonys
  , IR.ninjaDefaults, IR.ninjaPools

    -- * "Language.Ninja.IR.Meta"
  , IR.Meta, IR.makeMeta, IR.metaReqVersion, IR.metaBuildDir

    -- * "Language.Ninja.IR.Build"
  , IR.Build, IR.makeBuild, IR.buildRule, IR.buildOuts, IR.buildDeps

    -- * "Language.Ninja.IR.Rule"
  , IR.Rule, IR.makeRule
  , IR.ruleName, IR.ruleCommand, IR.ruleDescription, IR.rulePool, IR.ruleDepfile
  , IR.ruleSpecialDeps, IR.ruleGenerator, IR.ruleRestat, IR.ruleResponseFile

  , IR.SpecialDeps, IR.makeSpecialDepsGCC, IR.makeSpecialDepsMSVC
  , IR._SpecialDepsGCC, IR._SpecialDepsMSVC

  , IR.ResponseFile, IR.makeResponseFile
  , IR.responseFilePath, IR.responseFileContent

    -- * "Language.Ninja.IR.Target"
  , IR.Target, IR.makeTarget
  , IR.targetIText, IR.targetText

  , IR.Output, IR.makeOutput, IR.outputTarget

  , IR.Dependency, IR.makeDependency, IR.dependencyTarget, IR.dependencyType
  , IR.DependencyType (..), IR._NormalDependency, IR._OrderOnlyDependency

    -- * "Language.Ninja.IR.Pool"
  , IR.Pool
  , IR.makePool, IR.makePoolDefault, IR.makePoolConsole, IR.makePoolCustom
  , IR.poolName, IR.poolDepth

  , IR.PoolName
  , IR.makePoolNameDefault, IR.makePoolNameConsole, IR.makePoolNameCustom
  , IR._PoolNameDefault, IR._PoolNameConsole, IR._PoolNameCustom
  , IR.poolNameText, IR.printPoolName, IR.parsePoolName

  , IR.PoolDepth
  , IR.makePoolDepth, IR.makePoolInfinite
  , IR.poolDepthPositive
  ) where

import           Language.Ninja.IR.Build  as IR
import           Language.Ninja.IR.Meta   as IR
import           Language.Ninja.IR.Ninja  as IR
import           Language.Ninja.IR.Pool   as IR
import           Language.Ninja.IR.Rule   as IR
import           Language.Ninja.IR.Target as IR
