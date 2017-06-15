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
{-# OPTIONS_HADDOCK #-}

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

import qualified Control.Arrow         as Arr

import           Control.Lens.Getter

import           Language.Ninja.Types

import qualified Language.Ninja.Env    as Ninja
import qualified Language.Ninja.Types  as Ninja

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import qualified Data.HashMap.Strict   as HM

import           Data.Char
import           Data.Monoid

import           Flow

prettyNinja :: PNinja -> Text
prettyNinja ninja
  = [ map prettyRule     (HM.toList (ninja ^. pninjaRules))
    , map prettySingle   (HM.toList (ninja ^. pninjaSingles))
    , map prettyMultiple (HM.toList (ninja ^. pninjaMultiples))
    , map prettyPhony    (HM.toList (ninja ^. pninjaPhonys))
    , map prettyDefault  (HS.toList (ninja ^. pninjaDefaults))
    , map prettyPool     (HM.toList (ninja ^. pninjaPools))
    ] |> mconcat |> mconcat

prettyRule :: (Text, PRule) -> Text
prettyRule (name, rule) = do
  let binds = rule ^. pruleBind
              |> HM.toList
              |> map (Arr.second prettyExpr .> prettyBind)
              |> mconcat
  mconcat ["rule ", name, "\n", binds]

prettyExpr :: Ninja.PExpr -> Text
prettyExpr = go .> mconcat
  where
    go (Ninja.PExprs es) = map prettyExpr es
    go (Ninja.PLit  str) = [str]
    go (Ninja.PVar name) = ["${", name, "}"]

prettySingle :: (FileText, PBuild) -> Text
prettySingle (output, build) = prettyMultiple (HS.singleton output, build)

prettyMultiple :: (HashSet FileText, PBuild) -> Text
prettyMultiple (outputs, build) = do
  let stack = Ninja.getEnvStack (build ^. pbuildEnv)

  let prefixIfThere :: Text -> Text -> Text
      prefixIfThere pfx rest = if T.all isSpace rest then "" else pfx <> rest

  let unwordsSet :: HashSet Text -> Text
      unwordsSet = HS.toList .> T.unwords

  let ruleName  = build ^. pbuildRule
  let normal    = build ^. pbuildDeps . pdepsNormal
  let implicit  = build ^. pbuildDeps . pdepsImplicit
  let orderOnly = build ^. pbuildDeps . pdepsOrderOnly
  let binds     = build ^. pbuildBind

  mconcat
    [ "build ", T.unwords (HS.toList outputs), ": "
    , ruleName, " ", unwordsSet normal
    , prefixIfThere " | "  (unwordsSet implicit)
    , prefixIfThere " || " (unwordsSet orderOnly), "\n"
    , "    # environment: ", tshow (map HM.toList stack), "\n"
    , HM.toList binds |> map prettyBind |> mconcat
    ]

prettyPhony :: (Text, HashSet FileText) -> Text
prettyPhony (name, inputs)
  = [ ["build ", name, ": phony ", T.unwords (HS.toList inputs)]
    ] |> map mconcat |> T.unlines

prettyDefault :: FileText -> Text
prettyDefault target
  = [ ["default ", target]
    ] |> map mconcat |> T.unlines

prettyPool :: (Text, Int) -> Text
prettyPool (name, depth)
  = [ ["pool ", name]
    , ["    depth = ", tshow depth]
    ] |> map mconcat |> T.unlines

prettyBind :: (Text, Text) -> Text
prettyBind (name, value) = mconcat ["    ", name, " = ", value, "\n"]

tshow :: (Show s) => s -> Text
tshow = show .> T.pack
