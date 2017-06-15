{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Monoid

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

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

import qualified Turtle

dataPrefix :: String
dataPrefix = "" -- "tests/data/"

roundtrip :: String -> H.Expectation
roundtrip file = do
  let inputPath = dataPrefix <> file <> ".ninja"
  let expectedPath = dataPrefix <> file <> ".expected"

  let withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    input  <- Ninja.parse inputPath
    let prettyInput = Ninja.prettyNinja input
    let tmpfile = tmpdir </> "generated.ninja"
    Turtle.writeTextFile tmpfile prettyInput
    output <- Ninja.parse (FP.encodeString tmpfile)
    -- prettyOutput <- Ninja.prettyNinja output
    pure (input, output)

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
main = test -- generateExpectedRoundtrip
