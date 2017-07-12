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

{-# LANGUAGE OverloadedStrings #-}

-- |
--   Module      : Language.Ninja.Pretty
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A rudimentary pretty-printer for 'AST.Ninja'.
--
--   @since 0.1.0
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

import           Control.Arrow       (second)

import qualified Control.Lens        as Lens

import qualified Language.Ninja.AST  as AST

import qualified Data.HashMap.Strict as HM

import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS

import           Data.Text           (Text)
import qualified Data.Text           as Text

import           Data.Char           (isSpace)
import           Data.Monoid         ((<>))

import           Flow                ((.>), (|>))

--------------------------------------------------------------------------------

-- | Pretty-print a 'AST.Ninja'.
--
--   @since 0.1.0
prettyNinja :: AST.Ninja () -> Text
prettyNinja ninja
  = [ map prettyRule     (HM.toList (Lens.view AST.ninjaRules     ninja))
    , map prettySingle   (HM.toList (Lens.view AST.ninjaSingles   ninja))
    , map prettyMultiple (HM.toList (Lens.view AST.ninjaMultiples ninja))
    , map prettyPhony    (HM.toList (Lens.view AST.ninjaPhonys    ninja))
    , map prettyDefault  (HS.toList (Lens.view AST.ninjaDefaults  ninja))
    , map prettyPool     (HM.toList (Lens.view AST.ninjaPools     ninja))
    ] |> mconcat |> mconcat

-- | Pretty-print an 'AST.Expr'
--
--   @since 0.1.0
prettyExpr :: AST.Expr () -> Text
prettyExpr = go .> mconcat
  where
    go (AST.Exprs _   es) = map prettyExpr es
    go (AST.Lit   _ text) = [text]
    go (AST.Var   _ name) = ["${", name, "}"]

-- | Pretty-print a Ninja @rule@ declaration.
--
--   @since 0.1.0
prettyRule :: (Text, AST.Rule ()) -> Text
prettyRule (name, rule) = do
  let binds = Lens.view AST.ruleBind rule
              |> HM.toList
              |> map (second prettyExpr .> prettyBind)
              |> mconcat
  mconcat ["rule ", name, "\n", binds]

-- | Pretty-print a Ninja @build@ declaration with one output.
--
--   @since 0.1.0
prettySingle :: (Text, AST.Build ()) -> Text
prettySingle (output, build) = prettyMultiple (HS.singleton output, build)

-- | Pretty-print a Ninja @build@ declaration with multiple outputs.
--
--   @since 0.1.0
prettyMultiple :: (HashSet Text, AST.Build ()) -> Text
prettyMultiple (outputs, build) = do
  let prefixIfThere :: Text -> Text -> Text
      prefixIfThere pfx rest = if Text.all isSpace rest then "" else pfx <> rest

  let unwordsSet :: HashSet Text -> Text
      unwordsSet = HS.toList .> Text.unwords

  let ruleName  = Lens.view AST.buildRule build
  let deps      = Lens.view AST.buildDeps build
  let binds     = Lens.view AST.buildBind build
  let normal    = Lens.view AST.depsNormal    deps
  let implicit  = Lens.view AST.depsImplicit  deps
  let orderOnly = Lens.view AST.depsOrderOnly deps

  mconcat
    [ "build ", Text.unwords (HS.toList outputs), ": "
    , ruleName, " ", unwordsSet normal
    , prefixIfThere " | "  (unwordsSet implicit)
    , prefixIfThere " || " (unwordsSet orderOnly), "\n"
    , HM.toList binds |> map prettyBind |> mconcat
    ]

-- | Pretty-print a Ninja phony @build@ declaration.
--
--   @since 0.1.0
prettyPhony :: (Text, HashSet Text) -> Text
prettyPhony (name, inputs)
  = [ ["build ", name, ": phony ", Text.unwords (HS.toList inputs)]
    ] |> map mconcat |> Text.unlines

-- | Pretty-print a Ninja @default@ declaration.
--
--   @since 0.1.0
prettyDefault :: Text -> Text
prettyDefault target
  = [ ["default ", target]
    ] |> map mconcat |> Text.unlines

-- | Pretty-print a Ninja @pool@ declaration.
--
--   @since 0.1.0
prettyPool :: (Text, Int) -> Text
prettyPool (name, depth)
  = [ ["pool ", name]
    , ["    depth = ", tshow depth]
    ] |> map mconcat |> Text.unlines

-- | Pretty-print a Ninja indented binding.
--
--   @since 0.1.0
prettyBind :: (Text, Text) -> Text
prettyBind (name, value) = mconcat ["    ", name, " = ", value, "\n"]

--------------------------------------------------------------------------------

tshow :: (Show s) => s -> Text
tshow = show .> Text.pack

--------------------------------------------------------------------------------
