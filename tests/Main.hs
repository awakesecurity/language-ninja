module Main (main) where

import           Data.Monoid

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Language.Ninja             as Ninja

import qualified Test.Hspec                 as H

dataPrefix :: String
dataPrefix = "" -- "tests/data/"

liftE :: IO a -> ExceptT String IO a
liftE action = let handler :: SomeException -> IO (Either String a)
                   handler = pure . Left . displayException
               in ExceptT ((pure <$> action) `catch` handler)

roundtrip :: String -> H.Expectation
roundtrip file = H.shouldReturn (runExceptT go) (Right ())
  where
    go :: ExceptT String IO ()
    go = do
      let inputPath = dataPrefix <> file <> ".ninja"
      let expectedPath = dataPrefix <> file <> ".expected"
      input <- liftE (Ninja.parse inputPath)
      actual <- liftE (Ninja.prettyNinja input)
      expected <- liftE (BS.readFile expectedPath)
      unless (actual == expected) $ do
        throwE "output was not equal"

generateExpectedRoundtrip :: IO ()
generateExpectedRoundtrip = forM_ roundtripNames go
  where
    go :: String -> IO ()
    go file = do
      let inputPath = dataPrefix <> file <> ".ninja"
      let expectedPath = dataPrefix <> file <> ".expected"
      parsed <- Ninja.parse inputPath
      prettied <- Ninja.prettyNinja parsed
      BS.writeFile expectedPath prettied

roundtripNames :: [String]
roundtripNames -- = ["test6"]
  = [ "buildseparate"
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
