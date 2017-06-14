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

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import qualified Data.HashMap.Strict   as HM

import           Data.Char
import           Data.Monoid

import           Flow

prettyNinja :: PNinja -> IO ByteString
prettyNinja ninja
  = [ mapM prettyRule     (ninja ^. pninjaRules)
    , mapM prettySingle   (ninja ^. pninjaSingles)
    , mapM prettyMultiple (ninja ^. pninjaMultiples)
    , mapM prettyPhony    (ninja ^. pninjaPhonys)
    , mapM prettyDefault  (ninja ^. pninjaDefaults)
    , mapM prettyPool     (ninja ^. pninjaPools)
    ] |> sequenceA |> fmap (mconcat .> mconcat)

prettyRule :: (Str, PRule) -> IO ByteString
prettyRule (name, (Ninja.MkPRule {..})) = do
  let binds = mconcat $ map (prettyBind . Arr.second prettyExpr) ruleBind
  pure $ mconcat ["rule ", name, "\n", binds]

prettyExpr :: Ninja.PExpr -> ByteString
prettyExpr = go .> mconcat
  where
    go (Ninja.PExprs es) = map prettyExpr es
    go (Ninja.PLit  str) = [str]
    go (Ninja.PVar name) = ["${", name, "}"]

prettySingle :: (FileStr, PBuild) -> IO ByteString
prettySingle (output, build) = prettyMultiple ([output], build)

prettyMultiple :: ([FileStr], PBuild) -> IO ByteString
prettyMultiple (outputs, (Ninja.MkPBuild {..})) = do
  stack <- Ninja.getEnvStack env

  let prefixIfThere :: Str -> Str -> Str
      prefixIfThere pfx rest = if BSC8.all isSpace rest then "" else pfx <> rest

  let normal = BSC8.unwords depsNormal
  let implicit = BSC8.unwords depsImplicit
  let orderOnly = BSC8.unwords depsOrderOnly
  let binds = mconcat (map prettyBind buildBind)

  pure $ mconcat
    [ "build ", BSC8.unwords outputs, ": "
    , ruleName, " ", normal
    , prefixIfThere " | "  implicit
    , prefixIfThere " || " orderOnly, "\n"
    , "    # environment: ", bshow (map HM.toList stack), "\n"
    , binds
    ]

prettyPhony :: (Str, [FileStr]) -> IO ByteString
prettyPhony (name, inputs)
  = [ ["build ", name, ": phony ", BSC8.unwords inputs]
    ] |> map mconcat |> BSC8.unlines |> pure

prettyDefault :: FileStr -> IO ByteString
prettyDefault target
  = [ ["default ", target]
    ] |> map mconcat |> BSC8.unlines |> pure

prettyPool :: (Str, Int) -> IO ByteString
prettyPool (name, depth)
  = [ ["pool ", name]
    , ["    depth = ", bshow depth]
    ] |> map mconcat |> BSC8.unlines |> pure

prettyBind :: (Str, Str) -> Str
prettyBind (name, value) = mconcat ["    ", name, " = ", value, "\n"]

bshow :: (Show s) => s -> Str
bshow = T.encodeUtf8 . T.pack . show
