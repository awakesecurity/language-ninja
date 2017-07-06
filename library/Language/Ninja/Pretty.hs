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
--   A rudimentary pretty-printer for 'AST.Ninja'.
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

import           Language.Ninja.AST    (FileText)

import qualified Language.Ninja.AST    as AST

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

import           Flow                  ((.>), (|>))

--------------------------------------------------------------------------------

-- | Pretty-print a 'AST.Ninja'.
prettyNinja :: AST.Ninja () -> Text
prettyNinja ninja
  = [ map prettyRule     (HM.toList (ninja ^. AST.ninjaRules))
    , map prettySingle   (HM.toList (ninja ^. AST.ninjaSingles))
    , map prettyMultiple (HM.toList (ninja ^. AST.ninjaMultiples))
    , map prettyPhony    (HM.toList (ninja ^. AST.ninjaPhonys))
    , map prettyDefault  (HS.toList (ninja ^. AST.ninjaDefaults))
    , map prettyPool     (HM.toList (ninja ^. AST.ninjaPools))
    ] |> mconcat |> mconcat

-- | Pretty-print an 'AST.Expr'
prettyExpr :: AST.Expr () -> Text
prettyExpr = go .> mconcat
  where
    go (AST.Exprs _   es) = map prettyExpr es
    go (AST.Lit   _ text) = [text]
    go (AST.Var   _ name) = ["${", name, "}"]

-- | Pretty-print a Ninja @rule@ declaration.
prettyRule :: (Text, AST.Rule ()) -> Text
prettyRule (name, rule) = do
  let binds = rule ^. AST.ruleBind
              |> HM.toList
              |> map (Arr.second prettyExpr .> prettyBind)
              |> mconcat
  mconcat ["rule ", name, "\n", binds]

-- | Pretty-print a Ninja @build@ declaration with one output.
prettySingle :: (FileText, AST.Build ()) -> Text
prettySingle (output, build) = prettyMultiple (HS.singleton output, build)

-- | Pretty-print a Ninja @build@ declaration with multiple outputs.
prettyMultiple :: (HashSet FileText, AST.Build ()) -> Text
prettyMultiple (outputs, build) = do
  let prefixIfThere :: Text -> Text -> Text
      prefixIfThere pfx rest = if T.all isSpace rest then "" else pfx <> rest

  let unwordsSet :: HashSet Text -> Text
      unwordsSet = HS.toList .> T.unwords

  let ruleName  = build ^. AST.buildRule
  let deps      = build ^. AST.buildDeps
  let binds     = build ^. AST.buildBind
  let normal    = deps ^. AST.depsNormal
  let implicit  = deps ^. AST.depsImplicit
  let orderOnly = deps ^. AST.depsOrderOnly

  mconcat
    [ "build ", T.unwords (HS.toList outputs), ": "
    , ruleName, " ", unwordsSet normal
    , prefixIfThere " | "  (unwordsSet implicit)
    , prefixIfThere " || " (unwordsSet orderOnly), "\n"
    , HM.toList binds |> map prettyBind |> mconcat
    ]

-- | Pretty-print a Ninja phony @build@ declaration.
prettyPhony :: (Text, HashSet FileText) -> Text
prettyPhony (name, inputs)
  = [ ["build ", name, ": phony ", T.unwords (HS.toList inputs)]
    ] |> map mconcat |> T.unlines

-- | Pretty-print a Ninja @default@ declaration.
prettyDefault :: FileText -> Text
prettyDefault target
  = [ ["default ", target]
    ] |> map mconcat |> T.unlines

-- | Pretty-print a Ninja @pool@ declaration.
prettyPool :: (Text, Int) -> Text
prettyPool (name, depth)
  = [ ["pool ", name]
    , ["    depth = ", tshow depth]
    ] |> map mconcat |> T.unlines

-- | Pretty-print a Ninja indented binding.
prettyBind :: (Text, Text) -> Text
prettyBind (name, value) = mconcat ["    ", name, " = ", value, "\n"]

--------------------------------------------------------------------------------

tshow :: (Show s) => s -> Text
tshow = show .> T.pack

--------------------------------------------------------------------------------
