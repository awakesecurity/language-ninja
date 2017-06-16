-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Main.hs
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

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Language.Ninja             as Ninja

import qualified Test.Hspec                 as H
import qualified Test.HUnit                 as H

import           Filesystem.Path.CurrentOS  ((</>))
import qualified Filesystem.Path.CurrentOS  as FP

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Diff            as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson

import qualified Turtle

dataPrefix :: String
dataPrefix = "./tests/data/"

roundtrip :: String -> H.Expectation
roundtrip file = do
  old <- Turtle.pwd

  Turtle.cd (FP.decodeString dataPrefix)

  let inputPath = file <> ".ninja"
  let expectedPath = file <> ".expected"

  let withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    input  <- Ninja.parse inputPath
    let prettyInput = Ninja.prettyNinja input
    let tmpfile = tmpdir </> "generated.ninja"
    Turtle.writeTextFile tmpfile prettyInput
    output <- Ninja.parse (FP.encodeString tmpfile)
    let prettyOutput = Ninja.prettyNinja output
    pure (prettyInput, prettyOutput)

  Turtle.cd old

  unless (actual == expected) $ do
    -- let actualJ   = Aeson.toJSON actual
    -- let expectedJ = Aeson.toJSON expected
    -- -- LBSC8.putStrLn (Aeson.encodePretty (Aeson.diff actualJ expectedJ))
    -- LBSC8.putStrLn (Aeson.encodePretty expectedJ)
    -- LBSC8.putStrLn (Aeson.encodePretty actualJ)
    -- Aeson.encode actualJ `H.shouldBe` Aeson.encode expectedJ
    actual `H.shouldBe` expected

generateExpectedRoundtrip :: IO ()
generateExpectedRoundtrip = forM_ roundtripNames go
  where
    go :: String -> IO ()
    go file = do
      let inputPath = dataPrefix <> file <> ".ninja"
      let expectedPath = dataPrefix <> file <> ".expected"
      parsed <- Ninja.parse inputPath
      let prettied = Ninja.prettyNinja parsed
      T.writeFile expectedPath prettied

roundtripNames :: [String]
roundtripNames = [ "buildseparate"
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

test :: IO ()
test = H.hspec $ do
  H.describe "Language.Ninja.Parse.parse" $ do
    let roundtripTest name = H.it ("roundtrip on " <> name) (roundtrip name)
    forM_ roundtripNames roundtripTest

main :: IO ()
main = do
  Turtle.view (Turtle.ls ".")
  test -- generateExpectedRoundtrip
