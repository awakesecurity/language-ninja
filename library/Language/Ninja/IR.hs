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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.IR
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The Ninja build language intermediate representation after compilation.
module Language.Ninja.IR
  ( module Language.Ninja.IR.Build
  , module Language.Ninja.IR.Meta
  , module Language.Ninja.IR.Ninja
  , module Language.Ninja.IR.Pool
  , module Language.Ninja.IR.Rule
  , module Language.Ninja.IR.Target
  , module Language.Ninja.Misc.Command
  , module Language.Ninja.Misc.IText
  , module Language.Ninja.Misc.Path
  ) where

import           Language.Ninja.IR.Build
import           Language.Ninja.IR.Meta
import           Language.Ninja.IR.Ninja
import           Language.Ninja.IR.Pool
import           Language.Ninja.IR.Rule
import           Language.Ninja.IR.Target
import           Language.Ninja.Misc.Command
import           Language.Ninja.Misc.IText
import           Language.Ninja.Misc.Path
