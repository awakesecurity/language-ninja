-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Pretty.hs
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.Pretty
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Pretty
  ( module Language.Ninja.Pretty -- FIXME: specific export list
  ) where

import           Language.Ninja.Types  (Build, FileStr, Ninja, Rule, Str)
import qualified Language.Ninja.Types  as Ninja

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (unlines, unwords)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

prettyNinja :: Ninja -> ByteString
prettyNinja ninja
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettyRule :: (Str, Rule) -> ByteString
prettyRule (name, (Ninja.MkRule {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettySingle :: (FileStr, Build) -> ByteString
prettySingle (file, (Ninja.MkBuild {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettyMultiple :: ([FileStr], Build) -> ByteString
prettyMultiple (files, (Ninja.MkBuild {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettyPhony :: (Str, [FileStr]) -> ByteString
prettyPhony (name, files)
  = BS.unlines $ map mconcat
    [ ["build ", name, ": phony ", BS.unwords files]
    ]

prettyDefault :: FileStr -> ByteString
prettyDefault def
  = BS.unlines $ map mconcat
    [ ["default ", def]
    ]

prettyPool :: (Str, Int) -> ByteString
prettyPool (name, depth)
  = BS.unlines $ map mconcat
    [ ["pool ", name]
    , [" depth = ", bshow depth]
    ]

bshow :: (Show s) => s -> Str
bshow = T.encodeUtf8 . T.pack . show
