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

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC8

import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Identity      (Identity)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Control.Lens                as Lens

import qualified Language.Ninja.Compile      as Ninja
import qualified Language.Ninja.Misc.Command as Ninja
import qualified Language.Ninja.Misc.IText   as Ninja
import qualified Language.Ninja.Misc.Path    as Ninja
import qualified Language.Ninja.Parse        as Ninja
import qualified Language.Ninja.Pretty       as Ninja

import qualified Language.Ninja.IR           as IR

import qualified Language.Ninja.AST          as AST
import qualified Language.Ninja.AST.Env      as AST
import qualified Language.Ninja.AST.Expr     as AST
import qualified Language.Ninja.AST.Rule     as AST

import qualified Test.Tasty                  as T
import qualified Test.Tasty.Golden           as T
import qualified Test.Tasty.HUnit            as T
import qualified Test.Tasty.Ingredients      as T
import qualified Test.Tasty.Options          as T
import qualified Test.Tasty.Runners.Html     as T
import qualified Test.Tasty.SmallCheck       as T

import qualified Test.Tasty.Lens.Iso         as T.Iso
import qualified Test.Tasty.Lens.Lens        as T.Lens
import qualified Test.Tasty.Lens.Prism       as T.Prism

import qualified Test.SmallCheck.Series      as SC

import qualified Data.Versions               as Ver

import           Filesystem.Path.CurrentOS   ((</>))
import qualified Filesystem.Path.CurrentOS   as FP

import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Diff             as Aeson
import qualified Data.Aeson.Encode.Pretty    as Aeson

import           Data.Hashable               (Hashable)

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS

import qualified Data.List.NonEmpty          as NE

import qualified Turtle

import           Flow

import           Tests.Orphans               ()

--------------------------------------------------------------------------------

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

parseTestNinja :: String -> IO AST.Ninja
parseTestNinja name = do
  old <- Turtle.pwd
  Turtle.cd (FP.decodeString dataPrefix)
  result <- Ninja.parse (name <> ".ninja")
  Turtle.cd old
  pure result

roundtripTest :: AST.Ninja -> IO ()
roundtripTest ninja = do
  let withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    let prettyInput = Ninja.prettyNinja ninja
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

compileTest :: AST.Ninja -> IO ()
compileTest ninja = void $ do
  either (displayException .> fail) pure (Ninja.compile ninja)

ninjaTests :: String -> AST.Ninja -> T.TestTree
ninjaTests name ninja
  = T.testGroup ("Testing " <> name <> ".ninja")
    [ T.testCase "roundtrip through parser and pretty-printer" $ do
        roundtripTest ninja
    , T.testCase "compile to Ninja" $ do
        compileTest ninja
    ]

opticsTests :: T.TestTree
opticsTests
  = T.testGroup "Testing optics with SmallCheck"
    [ testModule "Language.Ninja.IR.Build"
      [ testType "Build" [] -- FIXME: combinatorial explosion
        -- [ testLens 1 "buildRule" IR.buildRule
        -- , testLens 1 "buildOuts" IR.buildOuts
        -- , testLens 1 "buildDeps" IR.buildDeps
        -- ]
      ]
    , testModule "Language.Ninja.IR.Meta"
      [ testType "Meta"
        [ testLens def "metaReqVersion" IR.metaReqVersion
        , testLens def "metaBuildDir"   IR.metaBuildDir
        ]
      ]
    , testModule "Language.Ninja.IR.Ninja"
      [ testType "Ninja" [] -- FIXME: combinatorial explosion
        -- [ testLens 1 "ninjaMeta"     IR.ninjaMeta
        -- , testLens 1 "ninjaBuilds"   IR.ninjaBuilds
        -- , testLens 1 "ninjaPhonys"   IR.ninjaPhonys
        -- , testLens 1 "ninjaDefaults" IR.ninjaDefaults
        -- , testLens 1 "ninjaPools"    IR.ninjaPools
        -- ]
      ]
    , testModule "Language.Ninja.IR.Pool"
      [ testType "Pool"
        [
        ]
      , testType "PoolName"
        [ testIso def "poolNameText" IR.poolNameText
        ]
      , testType "PoolDepth"
        [ testIso def "poolDepthPositive" IR.poolDepthPositive
        ]
      ]
    , testModule "Language.Ninja.IR.Rule"
      [ testType "Rule" [] -- FIXME: combinatorial explosion
        -- [ testLens 1 "ruleName"         IR.ruleName
        -- , testLens 1 "ruleCommand"      IR.ruleCommand
        -- , testLens 1 "ruleDescription"  IR.ruleDescription
        -- , testLens 1 "rulePool"         IR.rulePool
        -- , testLens 1 "ruleDepfile"      IR.ruleDepfile
        -- , testLens 1 "ruleSpecialDeps"  IR.ruleSpecialDeps
        -- , testLens 1 "ruleGenerator"    IR.ruleGenerator
        -- , testLens 1 "ruleRestat"       IR.ruleRestat
        -- , testLens 1 "ruleResponseFile" IR.ruleResponseFile
        -- ]
      , testType "SpecialDeps"
        [ testPrism def "_SpecialDepsGCC"  IR._SpecialDepsGCC
        , testPrism def "_SpecialDepsMSVC" IR._SpecialDepsMSVC
        ]
      , testType "ResponseFile"
        [ testLens def "responseFilePath"    IR.responseFilePath
        , testLens def "responseFileContent" IR.responseFileContent
        ]
      ]
    , testModule "Language.Ninja.IR.Target"
      [ testType "Target"
        [ testIso def "targetIText" IR.targetIText
        , testIso def "targetText"  IR.targetText
        ]
      , testType "Output"
        [ testLens def "outputTarget" IR.outputTarget
        , testLens def "outputType"   IR.outputType
        ]
      , testType "OutputType"
        [ testPrism def "_ExplicitOutput" IR._ExplicitOutput
        , testPrism def "_ImplicitOutput" IR._ImplicitOutput
        ]
      , testType "Dependency"
        [ testLens def "dependencyTarget" IR.dependencyTarget
        , testLens def "dependencyType"   IR.dependencyType
        ]
      , testType "DependencyType"
        [ testPrism def "_NormalDependency"    IR._NormalDependency
        , testPrism def "_ImplicitDependency"  IR._ImplicitDependency
        , testPrism def "_OrderOnlyDependency" IR._OrderOnlyDependency
        ]
      ]
    , testModule "Language.Ninja.Env"
      [ testType "Env"
        [ testIso 1 "fromEnv"
          (AST.fromEnv :: Lens.Iso' (AST.Env Text Int) (AST.Maps Text Int))
        ]
      ]
    , testModule "Language.Ninja.AST.Expr"
      [ testType "Expr"
        [ testPrism 4 "_Exprs" AST._Exprs
        , testPrism 4 "_Lit"   AST._Lit
        , testPrism 4 "_Var"   AST._Var
        ]
      ]
    , testModule "Language.Ninja.AST.Rule"
      [ testType "Rule"
        [ testLens 4 "ruleBind" AST.ruleBind
        ]
      ]
    , testModule "Language.Ninja.AST"
      [ testType "Ninja" [] -- FIXME: combinatorial explosion
        -- [ testLens 1 "ninjaRules"     AST.ninjaRules
        -- , testLens 1 "ninjaSingles"   AST.ninjaSingles
        -- , testLens 1 "ninjaMultiples" AST.ninjaMultiples
        -- , testLens 1 "ninjaPhonys"    AST.ninjaPhonys
        -- , testLens 1 "ninjaDefaults"  AST.ninjaDefaults
        -- , testLens 1 "ninjaSpecials"  AST.ninjaSpecials
        -- ]
      , testType "PBuild" [] -- FIXME: combinatorial explosion
        -- [ testLens 1 "pbuildRule" AST.pbuildRule
        -- , testLens 1 "pbuildEnv"  AST.pbuildEnv
        -- , testLens 1 "pbuildDeps" AST.pbuildDeps
        -- , testLens 1 "pbuildBind" AST.pbuildBind
        -- ]
      , testType "PDeps"
        [ testLens def "pdepsNormal"    AST.pdepsNormal
        , testLens def "pdepsImplicit"  AST.pdepsImplicit
        , testLens def "pdepsOrderOnly" AST.pdepsOrderOnly
        ]
      ]
    , testModule "Language.Ninja.Misc.Command"
      [ testType "Command"
        [ testIso def "commandText" Ninja.commandText
        ]
      ]
    , testModule "Language.Ninja.Misc.Path"
      [ testType "Path"
        [ testIso def "pathIText" Ninja.pathIText
        , testIso def "pathText"  Ninja.pathText
        ]
      ]
    , testModule "Language.Ninja.Misc.Located"
      [
      ]
    , testModule "Language.Ninja.Misc.IText"
      [ testType "IText"
        [ testIso def "itext" Ninja.itext
        ]
      ]
    ]
  where
    testIso   :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial Identity s, SC.Serial IO s, SC.CoSerial IO s
                 , SC.Serial Identity a, SC.Serial IO a, SC.CoSerial IO a
                 ) => Int -> T.TestName -> Lens.Iso' s a -> T.TestTree
    testLens  :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial IO s, SC.Serial IO a
                 , SC.Serial Identity a, SC.CoSerial IO a
                 ) => Int -> T.TestName -> Lens.Lens' s a -> T.TestTree
    testPrism :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial IO s, SC.Serial IO a
                 , SC.Serial Identity a, SC.CoSerial IO a
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
    def = (T.defaultValue :: T.SmallCheckDepth)
          |> toInteger |> fromIntegral

    typeTimeout :: T.Timeout
    typeTimeout = T.mkTimeout 20000000 -- 20 seconds

ingredients :: IO [T.Ingredient]
ingredients = [ [T.htmlRunner]
              , T.defaultIngredients
              ] |> mconcat |> pure

testTree :: IO T.TestTree
testTree = do
  ninjas <- forM testFiles parseTestNinja
  let tests = [ fmap (uncurry ninjaTests) (zip testFiles ninjas)
              , [opticsTests]
              ] |> mconcat
  pure (T.testGroup "language-ninja" tests)

test :: IO ()
test = do
  is <- ingredients
  tree <- testTree
  T.defaultMainWithIngredients is tree

main :: IO ()
main = do
  test

--------------------------------------------------------------------------------
