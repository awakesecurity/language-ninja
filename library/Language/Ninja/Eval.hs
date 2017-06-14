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
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.Eval
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   Evaluator for the Ninja build language.
module Language.Ninja.Eval
  ( module Language.Ninja.Eval -- FIXME: specific export list
  ) where

import           Language.Ninja.Eval.Build
import           Language.Ninja.Eval.Meta
import           Language.Ninja.Eval.Pool
import           Language.Ninja.Eval.Rule
import           Language.Ninja.Eval.Target
import           Language.Ninja.Misc.Command
import           Language.Ninja.Misc.IText
import           Language.Ninja.Misc.Path

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC8

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
