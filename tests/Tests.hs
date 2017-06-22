-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests.hs
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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Main (main) where

import           Data.Either
import           Data.Maybe
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
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Control.Lens               as Lens

import qualified Language.Ninja             as Ninja
import qualified Language.Ninja.AST         as Ninja
import qualified Language.Ninja.Env         as Ninja
import qualified Language.Ninja.Eval        as Ninja
import qualified Language.Ninja.Misc.IText  as Ninja
import qualified Language.Ninja.Misc.Path   as Ninja

import qualified Test.Tasty                 as T
import qualified Test.Tasty.Golden          as T
import qualified Test.Tasty.HUnit           as T
import qualified Test.Tasty.Ingredients     as T
import qualified Test.Tasty.Runners.Html    as T
import qualified Test.Tasty.SmallCheck      as T

import qualified Test.Tasty.Lens.Iso        as T.Iso
import qualified Test.Tasty.Lens.Lens       as T.Lens
import qualified Test.Tasty.Lens.Prism      as T.Prism

import           Test.SmallCheck.Series     as SC

import qualified Data.Versions              as Ver

import           Filesystem.Path.CurrentOS  ((</>))
import qualified Filesystem.Path.CurrentOS  as FP

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Diff            as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson

import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import qualified Data.List.NonEmpty         as NE

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
  = T.testGroup "Testing optics with SmallCheck" []
    -- [ testModule "Language.Ninja.Misc.AST.Build"
    --   [ testType "Build"
    --     [ testLens 1 "buildRule" Ninja.buildRule
    --     , testLens 1 "buildOuts" Ninja.buildOuts
    --     , testLens 1 "buildDeps" Ninja.buildDeps
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.AST.Meta"
    --   [ testType "Meta"
    --     [ testLens def "metaReqVersion" Ninja.metaReqVersion
    --     , testLens def "metaBuildDir"   Ninja.metaBuildDir
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.AST.Ninja"
    --   [ testType "Ninja"
    --     [ testLens 1 "ninjaMeta"     Ninja.ninjaMeta
    --     , testLens 1 "ninjaBuilds"   Ninja.ninjaBuilds
    --     , testLens 1 "ninjaPhonys"   Ninja.ninjaPhonys
    --     , testLens 1 "ninjaDefaults" Ninja.ninjaDefaults
    --     , testLens 1 "ninjaPools"    Ninja.ninjaPools
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.AST.Pool"
    --   [ testType "Pool"
    --     [
    --     ]
    --   , testType "PoolName"
    --     [ testPrism def "_PoolNameDefault" Ninja._PoolNameDefault
    --     , testPrism def "_PoolNameConsole" Ninja._PoolNameConsole
    --     , testPrism def "_PoolNameCustom"  Ninja._PoolNameCustom
    --     ]
    --   , testType "PoolDepth"
    --     [ testPrism def "_PoolDepth"    Ninja._PoolDepth
    --     , testPrism def "_PoolInfinite" Ninja._PoolInfinite
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.AST.Rule"
    --   [ testType "Rule"
    --     [ testLens 1 "ruleName"         Ninja.ruleName
    --     , testLens 1 "ruleCommand"      Ninja.ruleCommand
    --     , testLens 1 "ruleDescription"  Ninja.ruleDescription
    --     , testLens 1 "rulePool"         Ninja.rulePool
    --     , testLens 1 "ruleDepfile"      Ninja.ruleDepfile
    --     , testLens 1 "ruleSpecialDeps"  Ninja.ruleSpecialDeps
    --     , testLens 1 "ruleGenerator"    Ninja.ruleGenerator
    --     , testLens 1 "ruleRestat"       Ninja.ruleRestat
    --     , testLens 1 "ruleResponseFile" Ninja.ruleResponseFile
    --     ]
    --   , testType "SpecialDeps"
    --     [ testPrism def "_SpecialDepsGCC"  Ninja._SpecialDepsGCC
    --     , testPrism def "_SpecialDepsMSVC" Ninja._SpecialDepsMSVC
    --     ]
    --   , testType "ResponseFile"
    --     [ testLens def "responseFilePath"    Ninja.responseFilePath
    --     , testLens def "responseFileContent" Ninja.responseFileContent
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.AST.Target"
    --   [ testType "Target"
    --     [ testIso def "targetIText" Ninja.targetIText
    --     , testIso def "targetText"  Ninja.targetText
    --     ]
    --   , testType "Output"
    --     [ testLens def "outputTarget" Ninja.outputTarget
    --     , testLens def "outputType"   Ninja.outputType
    --     ]
    --   , testType "OutputType"
    --     [ testPrism def "_ExplicitOutput" Ninja._ExplicitOutput
    --     , testPrism def "_ImplicitOutput" Ninja._ImplicitOutput
    --     ]
    --   , testType "Dependency"
    --     [ testLens def "dependencyTarget" Ninja.dependencyTarget
    --     , testLens def "dependencyType"   Ninja.dependencyType
    --     ]
    --   , testType "DependencyType"
    --     [ testPrism def "_NormalDependency"    Ninja._NormalDependency
    --     , testPrism def "_ImplicitDependency"  Ninja._ImplicitDependency
    --     , testPrism def "_OrderOnlyDependency" Ninja._OrderOnlyDependency
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.Env"
    --   [ testType "Env"
    --     [ testIso 1 "fromEnv"
    --       (Ninja.fromEnv
    --        :: Lens.Iso' (Ninja.Env Text Int) (Ninja.Maps Text Int))
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.Types"
    --   [ testType "PNinja"
    --     [ testLens 1 "pninjaRules"     Ninja.pninjaRules
    --     , testLens 1 "pninjaSingles"   Ninja.pninjaSingles
    --     , testLens 1 "pninjaMultiples" Ninja.pninjaMultiples
    --     , testLens 1 "pninjaPhonys"    Ninja.pninjaPhonys
    --     , testLens 1 "pninjaDefaults"  Ninja.pninjaDefaults
    --     , testLens 1 "pninjaSpecials"  Ninja.pninjaSpecials
    --     ]
    --   , testType "PBuild"
    --     [ testLens 1 "pbuildRule" Ninja.pbuildRule
    --     , testLens 1 "pbuildEnv"  Ninja.pbuildEnv
    --     , testLens 1 "pbuildDeps" Ninja.pbuildDeps
    --     , testLens 1 "pbuildBind" Ninja.pbuildBind
    --     ]
    --   , testType "PDeps"
    --     [ testLens def "pdepsNormal"    Ninja.pdepsNormal
    --     , testLens def "pdepsImplicit"  Ninja.pdepsImplicit
    --     , testLens def "pdepsOrderOnly" Ninja.pdepsOrderOnly
    --     ]
    --   , testType "PRule"
    --     [ testLens 2 "pruleBind" Ninja.pruleBind
    --     ]
    --   , testType "PExpr"
    --     [ testPrism 3 "_PExprs" Ninja._PExprs
    --     , testPrism 3 "_PLit"   Ninja._PLit
    --     , testPrism 3 "_PVar"   Ninja._PVar
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.Command"
    --   [ testType "Command"
    --     [ testIso def "commandText" Ninja.commandText
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.Path"
    --   [ testType "Path"
    --     [ testIso def "pathIText" Ninja.pathIText
    --     , testIso def "pathText"  Ninja.pathText
    --     ]
    --   ]
    -- , testModule "Language.Ninja.Misc.Located"
    --   [
    --   ]
    -- , testModule "Language.Ninja.Misc.IText"
    --   [ testType "IText"
    --     [ testIso def "itext" Ninja.itext
    --     ]
    --   ]
    -- ]
  where
    testIso   :: ( Eq s, Eq a, Show s, Show a
                 , Serial Identity s, Serial IO s, CoSerial IO s
                 , Serial Identity a, Serial IO a, CoSerial IO a
                 ) => Int -> T.TestName -> Lens.Iso' s a -> T.TestTree
    testLens  :: ( Eq s, Eq a, Show s, Show a
                 , Serial IO s, Serial IO a, Serial Identity a, CoSerial IO a
                 ) => Int -> T.TestName -> Lens.Lens' s a -> T.TestTree
    testPrism :: ( Eq s, Eq a, Show s, Show a
                 , Serial IO s, Serial IO a, Serial Identity a, CoSerial IO a
                 ) => Int -> T.TestName -> Lens.Prism' s a -> T.TestTree
    testIso   d name i = withDepth d $ T.testGroup name [T.Iso.test   i]
    testLens  d name l = withDepth d $ T.testGroup name [T.Lens.test  l]
    testPrism d name p = withDepth d $ T.testGroup name [T.Prism.test p]

    withDepth :: Int -> (T.TestTree -> T.TestTree)
    withDepth depth = T.localOption (T.SmallCheckDepth depth)

    testModule :: T.TestName -> [T.TestTree] -> T.TestTree
    testModule = T.testGroup

    testType :: T.TestName -> [T.TestTree] -> T.TestTree
    testType name subtrees = T.localOption typeTimeout
                             $ T.testGroup name subtrees

    def :: Int
    def = 5

    typeTimeout :: T.Timeout
    typeTimeout = T.mkTimeout 20000000 -- 20 seconds

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

instance (Monad m) => SC.Serial m Ver.Version where
  series = foldr1 (\/) (map pure testVersions)

instance (Monad m, SC.CoSerial m Ver.VUnit) => SC.CoSerial m Ver.Version where
  coseries = coseries
             .> fmap (\f -> \(Ver.Version {..}) -> f (_vEpoch, _vChunks, _vRel))

instance (Monad m) => SC.CoSerial m Ver.VUnit where
  coseries = coseries
             .> fmap (\f -> \case (Ver.Digits i) -> f (Right i)
                                  (Ver.Str    s) -> f (Left  s))

instance (Monad m) => SC.Serial m Text where
  series = pure "" \/ pure "foo" \/ pure "42" \/ pure " "

instance (Monad m) => SC.CoSerial m Text where
  coseries rs = alts0 rs >>- \y -> alts2 rs >>- \f -> do
    pure (Text.uncons
          .> (\case Nothing      -> y
                    Just (b, bs) -> f (Text.singleton b) bs))

instance (Monad m, Serial m a) => Serial m (NE.NonEmpty a) where
  series = series |> fmap (SC.getNonEmpty .> NE.fromList)

instance (Monad m, CoSerial m a) => CoSerial m (NE.NonEmpty a) where
  coseries = coseries .> fmap (\f -> NE.toList .> f)

instance (Monad m, Serial m a, Eq a, Hashable a) => Serial m (HashSet a) where
  series = pure HS.empty
           \/ (HS.singleton <$> series)
           -- pure HS.empty
           -- \/ (HS.singleton <$> series)
           -- \/ (HS.union <$> series <~> series)

instance ( Monad m, CoSerial m a, Eq a, Hashable a
         ) => CoSerial m (HashSet a) where
  coseries = SC.coseries .> fmap (\f -> HS.toList .> f)

instance ( Monad m, Serial m k, Serial m v, Eq k, Hashable k
         ) => Serial m (HashMap k v) where
  series = pure HM.empty
           \/ (HM.singleton <$> series <~> series)
           -- pure HM.empty
           -- \/ (HM.singleton <$> series <~> series)
           -- \/ (HM.union <$> series <~> series)

instance ( Monad m, CoSerial m k, CoSerial m v, Eq k, Hashable k
         ) => CoSerial m (HashMap k v) where
  coseries = SC.coseries .> fmap (\f -> HM.toList .> f)

--------------------------------------------------------------------------------

testVersions :: [Ver.Version]
testVersions = [ "0.1.0"
               , "0.2"
               , "0.2.0"
               , "0.2.0.0"
               , "0.25-2"
               , "0.9.9.9"
               , "1"
               , "1.0"
               , "1.0.0"
               , "1.0.0.0"
               , "1.0.0.1"
               , "1.0.0-alpha"
               , "1.0.0-alpha.1"
               , "1.0.0-alpha.beta"
               , "1.0.0-beta"
               , "1.0.0-beta.11"
               , "1.0.0-beta.2"
               , "1.0.0-rc.1"
               , "1:0.10.16-3"
               , "1.0rc0"
               , "1.0rc1"
               , "1.1"
               , "1:1.0"
               , "1:1.1"
               , "1:1.2.3-1"
               , "1.1rc1"
               , "1.2"
               , "1.2.2r1-1"
               , "1.2.3"
               , "1.2.3-1"
               , "1.2.3-alpha"
               , "1.2.3-alpha.2"
               , "1.2.3r1"
               , "1.2.3r1-1"
               , "1.2.4"
               , "1.2.4r1-1"
               , "1.2-5"
               , "1.58.0-3"
               , "2"
               , "20150826-1"
               , "21-2"
               , "2.3.4"
               , "3.4.5"
               , "44.0.2403.157-1"
               , "7.1p1-1"
               , "8.u51-1"
               ] |> map (Ver.version .> fromRight)
  where
    fromRight :: (Show a, Show b) => Either a b -> b
    fromRight (Left  a) = error ("fromRight: " <> show a)
    fromRight (Right b) = b

--------------------------------------------------------------------------------
