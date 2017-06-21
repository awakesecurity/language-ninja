-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests.hs
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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Main (main) where

import           Data.Monoid

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Language.Ninja             as Ninja
import qualified Language.Ninja.Eval        as Ninja
import qualified Language.Ninja.Misc.IText  as Ninja
import qualified Language.Ninja.Misc.Path   as Ninja

import qualified Test.Tasty                 as T
import qualified Test.Tasty.Golden          as T
import qualified Test.Tasty.HUnit           as T
import qualified Test.Tasty.Ingredients     as T
import qualified Test.Tasty.Runners.Html    as T

import qualified Test.Tasty.Lens.Iso        as T.Iso
import qualified Test.Tasty.Lens.Lens       as T.Lens

import           Test.SmallCheck.Series     as SC

import           Filesystem.Path.CurrentOS  ((</>))
import qualified Filesystem.Path.CurrentOS  as FP

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Diff            as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson

import           Flow

import qualified Turtle

dataPrefix :: String
dataPrefix = "./tests/data/"

testFiles :: [String]
testFiles = [ "buildseparate"
            , "compdb"
            , "lexical"
            , "lint"
            , "nocreate"
            , "outputtouch"
            , "phonyorder"
            , "redefine"
            , "test1"
            , "test2"
            , "test3"
            , "test4"
            , "test5"
            , "test6"
            ]

parseTestNinja :: String -> IO Ninja.PNinja
parseTestNinja name = do
  old <- Turtle.pwd
  Turtle.cd (FP.decodeString dataPrefix)
  result <- Ninja.parse (name <> ".ninja")
  Turtle.cd old
  pure result

roundtripTest :: Ninja.PNinja -> IO ()
roundtripTest pninja = do
  let withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    let prettyInput = Ninja.prettyNinja pninja
    let tmpfile = tmpdir </> "generated.ninja"
    Turtle.writeTextFile tmpfile prettyInput
    output <- Ninja.parse (FP.encodeString tmpfile)
    let prettyOutput = Ninja.prettyNinja output
    pure (prettyInput, prettyOutput)

  unless (actual == expected) $ do
    -- let actualJ   = Aeson.toJSON actual
    -- let expectedJ = Aeson.toJSON expected
    -- -- LBSC8.putStrLn (Aeson.encodePretty (Aeson.diff actualJ expectedJ))
    -- LBSC8.putStrLn (Aeson.encodePretty expectedJ)
    -- LBSC8.putStrLn (Aeson.encodePretty actualJ)
    -- Aeson.encode actualJ `H.shouldBe` Aeson.encode expectedJ
    T.assertEqual "prefix" expected actual

evaluateTest :: Ninja.PNinja -> IO ()
evaluateTest pninja = void $ do
  Ninja.evaluate pninja

pninjaTests :: String -> Ninja.PNinja -> T.TestTree
pninjaTests name pninja
  = T.testGroup ("Testing " <> name <> ".ninja")
    [ T.testCase "roundtrip through parser and pretty-printer" $ do
        roundtripTest pninja
    , T.testCase "evaluate to Ninja" $ do
        evaluateTest pninja
    ]

opticsTests :: T.TestTree
opticsTests
  = T.testGroup "Testing optics with SmallCheck"
    [ T.testGroup "Language.Ninja.Misc.IText"
      [ T.Iso.test Ninja.itext
      ]
    , T.testGroup "Language.Ninja.Misc.Path"
      [ T.Iso.test Ninja.pathIText
      , T.Iso.test Ninja.pathText
      ]
    ]

ingredients :: IO [T.Ingredient]
ingredients = [ [T.htmlRunner]
              , T.defaultIngredients
              ] |> mconcat |> pure

testTree :: IO T.TestTree
testTree = do
  ninjas <- forM testFiles parseTestNinja
  let tests = [ fmap (uncurry pninjaTests) (zip testFiles ninjas)
              , [opticsTests]
              ] |> mconcat
  pure (T.testGroup "Language.Ninja" tests)

test :: IO ()
test = do
  is <- ingredients
  tree <- testTree
  T.defaultMainWithIngredients is tree

main :: IO ()
main = do
  test

--------------------------------------------------------------------------------

-- Orphan instances

instance (Monad m) => SC.Serial m Text where
  series = SC.series |> fmap Text.pack

instance (Monad m) => SC.CoSerial m Text where
  coseries = SC.coseries .> fmap (\f -> Text.unpack .> f)

--------------------------------------------------------------------------------
