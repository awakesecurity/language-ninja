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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.AST
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The Ninja build language AST after normalization.
module Language.Ninja.AST
  ( module Language.Ninja.AST.Build
  , module Language.Ninja.AST.Meta
  , module Language.Ninja.AST.Ninja
  , module Language.Ninja.AST.Pool
  , module Language.Ninja.AST.Rule
  , module Language.Ninja.AST.Target
  , module Language.Ninja.Misc.Command
  , module Language.Ninja.Misc.IText
  , module Language.Ninja.Misc.Path
  ) where

import           Language.Ninja.AST.Build
import           Language.Ninja.AST.Meta
import           Language.Ninja.AST.Ninja
import           Language.Ninja.AST.Pool
import           Language.Ninja.AST.Rule
import           Language.Ninja.AST.Target
import           Language.Ninja.Misc.Command
import           Language.Ninja.Misc.IText
import           Language.Ninja.Misc.Path
