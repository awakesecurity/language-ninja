-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST.hs
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

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

-- |
--   Module      : Language.Ninja.AST
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The Ninja build language after parsing.
--
--   This module re-exports all of the modules under the "Language.Ninja.AST"
--   namespace for convenience.
--
--   It is recommended that you import it with the following style:
--
--   > import qualified Language.Ninja.AST as AST
--
--   @since 0.1.0
module Language.Ninja.AST
  ( -- * "Language.Ninja.AST.Ninja"
    AST.Ninja, AST.makeNinja
  , AST.ninjaRules
  , AST.ninjaSingles
  , AST.ninjaMultiples
  , AST.ninjaPhonys
  , AST.ninjaDefaults
  , AST.ninjaPools
  , AST.ninjaSpecials

    -- * "Language.Ninja.AST.Build"
  , AST.Build, AST.makeBuild
  , AST.buildRule, AST.buildEnv, AST.buildDeps, AST.buildBind

    -- * "Language.Ninja.AST.Deps"
  , AST.Deps, AST.makeDeps
  , AST.depsNormal, AST.depsImplicit, AST.depsOrderOnly

    -- * "Language.Ninja.AST.Rule"
  , AST.Rule, AST.makeRule
  , AST.ruleBind

    -- * "Language.Ninja.AST.Expr"
  , AST.Expr (..)
  , AST._Exprs, AST._Lit, AST._Var
  , AST.askVar, AST.askExpr, AST.addBind, AST.addBinds
  , AST.normalizeExpr

    -- * "Language.Ninja.AST.Env"
  , AST.Env
  , AST.makeEnv, AST.fromEnv, AST.addEnv, AST.askEnv, AST.scopeEnv
  ) where

import qualified Language.Ninja.AST.Build as AST
import qualified Language.Ninja.AST.Deps  as AST
import qualified Language.Ninja.AST.Env   as AST
import qualified Language.Ninja.AST.Expr  as AST
import qualified Language.Ninja.AST.Ninja as AST
import qualified Language.Ninja.AST.Rule  as AST

--------------------------------------------------------------------------------
