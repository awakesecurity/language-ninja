-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Pretty.hs
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.Pretty
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Pretty
  ( -- * Pretty-printers
    prettyNinja
  , prettyExpr
  , prettyRule
  , prettySingle
  , prettyMultiple
  , prettyPhony
  , prettyDefault
  , prettyPool
  , prettyBind
  ) where

import qualified Control.Arrow         as Arr

import           Control.Lens.Getter   ((^.))

import           Language.Ninja.Types  (FileText, PBuild, PNinja, PRule)

import qualified Language.Ninja.Env    as Ninja
import qualified Language.Ninja.Types  as Ninja

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import qualified Data.HashMap.Strict   as HM

import           Data.Char             (isSpace)
import           Data.Monoid           ((<>))

import           Flow                  ((|>), (.>))

-- | FIXME: doc
prettyNinja :: PNinja -> Text
prettyNinja ninja
  = [ map prettyRule     (HM.toList (ninja ^. Ninja.pninjaRules))
    , map prettySingle   (HM.toList (ninja ^. Ninja.pninjaSingles))
    , map prettyMultiple (HM.toList (ninja ^. Ninja.pninjaMultiples))
    , map prettyPhony    (HM.toList (ninja ^. Ninja.pninjaPhonys))
    , map prettyDefault  (HS.toList (ninja ^. Ninja.pninjaDefaults))
    , map prettyPool     (HM.toList (ninja ^. Ninja.pninjaPools))
    ] |> mconcat |> mconcat

-- | FIXME: doc
prettyExpr :: Ninja.PExpr -> Text
prettyExpr = go .> mconcat
  where
    go (Ninja.PExprs es) = map prettyExpr es
    go (Ninja.PLit  str) = [str]
    go (Ninja.PVar name) = ["${", name, "}"]

-- | FIXME: doc
prettyRule :: (Text, PRule) -> Text
prettyRule (name, rule) = do
  let binds = rule ^. Ninja.pruleBind
              |> HM.toList
              |> map (Arr.second prettyExpr .> prettyBind)
              |> mconcat
  mconcat ["rule ", name, "\n", binds]

-- | FIXME: doc
prettySingle :: (FileText, PBuild) -> Text
prettySingle (output, build) = prettyMultiple (HS.singleton output, build)

-- | FIXME: doc
prettyMultiple :: (HashSet FileText, PBuild) -> Text
prettyMultiple (outputs, build) = do
  let prefixIfThere :: Text -> Text -> Text
      prefixIfThere pfx rest = if T.all isSpace rest then "" else pfx <> rest

  let unwordsSet :: HashSet Text -> Text
      unwordsSet = HS.toList .> T.unwords

  let ruleName  = build ^. Ninja.pbuildRule
  let normal    = build ^. Ninja.pbuildDeps . Ninja.pdepsNormal
  let implicit  = build ^. Ninja.pbuildDeps . Ninja.pdepsImplicit
  let orderOnly = build ^. Ninja.pbuildDeps . Ninja.pdepsOrderOnly
  let binds     = build ^. Ninja.pbuildBind

  mconcat
    [ "build ", T.unwords (HS.toList outputs), ": "
    , ruleName, " ", unwordsSet normal
    , prefixIfThere " | "  (unwordsSet implicit)
    , prefixIfThere " || " (unwordsSet orderOnly), "\n"
    , HM.toList binds |> map prettyBind |> mconcat
    ]

-- | FIXME: doc
prettyPhony :: (Text, HashSet FileText) -> Text
prettyPhony (name, inputs)
  = [ ["build ", name, ": phony ", T.unwords (HS.toList inputs)]
    ] |> map mconcat |> T.unlines

-- | FIXME: doc
prettyDefault :: FileText -> Text
prettyDefault target
  = [ ["default ", target]
    ] |> map mconcat |> T.unlines

-- | FIXME: doc
prettyPool :: (Text, Int) -> Text
prettyPool (name, depth)
  = [ ["pool ", name]
    , ["    depth = ", tshow depth]
    ] |> map mconcat |> T.unlines

-- | FIXME: doc
prettyBind :: (Text, Text) -> Text
prettyBind (name, value) = mconcat ["    ", name, " = ", value, "\n"]

tshow :: (Show s) => s -> Text
tshow = show .> T.pack
