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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import qualified Control.Arrow         as Arr

import           Control.Lens.Getter

import           Language.Ninja.Types

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import qualified Data.HashMap.Strict   as HM

import           Data.Char
import           Data.Monoid

import           Flow

import qualified Language.Ninja.AST    as NA
import qualified Language.Ninja.Parse  as NP
import qualified Language.Ninja.Types  as NP

--------------------------------------------------------------------------------

debugNinja :: IO NP.PNinja
debugNinja = NP.parse "data/build.ninja"

evaluate :: NP.PNinja -> NA.Ninja
evaluate = undefined

--------------------------------------------------------------------------------
