-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja.hs
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
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
--   Module      : Language.Ninja
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja
  ( module Exported
  ) where

import           Language.Ninja.AST    as Exported
import           Language.Ninja.Env    as Exported
import           Language.Ninja.Lexer  as Exported
import           Language.Ninja.Parse  as Exported
import           Language.Ninja.Pretty as Exported
import           Language.Ninja.Shake  as Exported
import           Language.Ninja.Types  as Exported
