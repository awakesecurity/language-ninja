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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The @language-ninja@ test suite.
module Main (main) where

import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))

import qualified Data.Typeable              as Ty

import           Data.Text                  (Text)

import           Control.Exception          (displayException)
import           Control.Monad              (forM, unless, void)
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Control.Lens               as Lens

import qualified Language.Ninja.AST.Env     as AST (Maps)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Compile     as Compile
import qualified Language.Ninja.Errors      as Errors
import qualified Language.Ninja.IR          as IR
import qualified Language.Ninja.Lexer       as Lexer
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Parser      as Parser
import qualified Language.Ninja.Pretty      as Pretty

import           Test.Tasty                 (TestName, TestTree)

import qualified Test.Tasty                 as Test
import qualified Test.Tasty.HUnit           as Test
import qualified Test.Tasty.Ingredients     as Test
import qualified Test.Tasty.Options         as Test
import qualified Test.Tasty.Runners.Html    as Test

import qualified Test.Tasty.QuickCheck      as Test.QC
import qualified Test.Tasty.SmallCheck      as Test.SC

import qualified Test.Tasty.Lens.Iso        as Test.Iso
import qualified Test.Tasty.Lens.Lens       as Test.Lens
import qualified Test.Tasty.Lens.Prism      as Test.Prism

import           Test.QuickCheck            ((===))
import qualified Test.QuickCheck            as QC
import           Test.QuickCheck.Instances  ()

import qualified Test.SmallCheck.Series     as SC

import           Filesystem.Path.CurrentOS  ((</>))
import qualified Filesystem.Path.CurrentOS  as FP

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import qualified Turtle

import           Flow                       ((.>), (|>))

import qualified Tests.Lint                 as Lint
import           Tests.Orphans              ()
import qualified Tests.ReferenceLexer       as RefLex

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

aesonSC' :: (Eq x, Show x)
         => SC.Series IO x
         -> (x -> Aeson.Value)
         -> (Aeson.Value -> Aeson.Parser x)
         -> TestTree
aesonSC' s toJ fromJ
  = Test.SC.testProperty "parseJSON . toJSON ≡ₛ pure"
    (Test.SC.over s (\x -> Aeson.parseEither fromJ (toJ x) == Right x))

aesonSC :: forall x.
           ( Eq x, Show x, SC.Serial IO x, Aeson.ToJSON x, Aeson.FromJSON x
           ) => Ty.Proxy x -> TestTree
aesonSC _ = aesonSC' @x SC.series Aeson.toJSON Aeson.parseJSON

aesonQC' :: (Eq x, Show x)
         => (QC.Gen x, x -> [x])
         -> (x -> Aeson.Value)
         -> (Aeson.Value -> Aeson.Parser x)
         -> TestTree
aesonQC' (gen, shrink) toJ fromJ
  = Test.QC.testProperty "parseJSON . toJSON ≡ₐ pure"
    (Test.QC.forAllShrink gen shrink
     (\x -> Aeson.parseEither fromJ (toJ x) === Right x))

aesonQC :: forall x.
           ( Eq x, Show x, QC.Arbitrary x, Aeson.ToJSON x, Aeson.FromJSON x
           ) => Ty.Proxy x -> TestTree
aesonQC _ = aesonQC' @x (QC.arbitrary, QC.shrink) Aeson.toJSON Aeson.parseJSON

parseTestNinja :: String -> IO (AST.Ninja ())
parseTestNinja name = do
  old <- Turtle.pwd
  Turtle.cd (FP.decodeString dataPrefix)
  let file = Lens.view (Lens.from Misc.pathString) (name <> ".ninja")
  result <- Parser.parseFileIO file >>= void .> pure
  Turtle.cd old
  pure result

lexerEquivalentTest :: String -> IO ()
lexerEquivalentTest name = do
  let file = dataPrefix <> name <> ".ninja"
             |> Lens.view (Lens.from Misc.pathString)

  let oldLexer :: Misc.Path -> ExceptT Errors.ParseError IO [Lexer.Lexeme ()]
      oldLexer = RefLex.lexerFile

  let newLexer :: Misc.Path -> ExceptT Errors.ParseError IO [Lexer.Lexeme ()]
      newLexer = Lexer.lexerFile .> fmap (map void)

  expected <- runExceptT (oldLexer file)
  actual   <- runExceptT (newLexer file)

  unless (expected == actual) $ do
    Test.assertEqual "prefix" expected actual

roundtripTest :: AST.Ninja () -> IO ()
roundtripTest ninja = do
  let withTempDir :: (FP.FilePath -> IO a) -> IO a
      withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    let prettyInput = Pretty.prettyNinja ninja
    let tmpfile = tmpdir </> "generated.ninja"
    Turtle.writeTextFile tmpfile prettyInput
    output <- Parser.parseFileIO (Lens.view (Lens.from Misc.pathFP) tmpfile)
              >>= void .> pure
    let prettyOutput = Pretty.prettyNinja output
    pure (prettyInput, prettyOutput)

  unless (expected == actual) $ do
    -- let actualJ   = Aeson.toJSON actual
    -- let expectedJ = Aeson.toJSON expected
    -- -- LBSC8.putStrLn (Aeson.encodePretty (Aeson.diff actualJ expectedJ))
    -- LBSC8.putStrLn (Aeson.encodePretty expectedJ)
    -- LBSC8.putStrLn (Aeson.encodePretty actualJ)
    -- Aeson.encode actualJ `H.shouldBe` Aeson.encode expectedJ
    Test.assertEqual "prefix" expected actual

compileTest :: AST.Ninja () -> IO ()
compileTest ninja = void $ do
  either (displayException .> fail) pure (Compile.compile ninja)

ninjaTests :: String -> AST.Ninja () -> TestTree
ninjaTests name ninja
  = Test.testGroup (name <> ".ninja")
    [ Test.testCase "compare lexer against reference implementation" $ do
        lexerEquivalentTest name
    , Test.testCase "roundtrip through parser and pretty-printer" $ do
        roundtripTest ninja
    , Test.testCase "compile to Ninja" $ do
        compileTest ninja
    ]

aesonTests :: TestTree
aesonTests
  = Test.testGroup "aeson"
    [ testModule "Language.Ninja.Lexer"
      [ testAesonSC 2   (Ty.Proxy @(Lexer.Lexeme Bool))
      , testAesonSC 4   (Ty.Proxy @(Lexer.LName  Bool))
      , testAesonSC 4   (Ty.Proxy @(Lexer.LFile  Bool))
      , testAesonSC 4   (Ty.Proxy @(Lexer.LBind  Bool))
      , testAesonSC 2   (Ty.Proxy @(Lexer.LBuild Bool))
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Build"
      [ testAesonSC 2   (Ty.Proxy @IR.Build)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Meta"
      [ testAesonSC def (Ty.Proxy @IR.Meta)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Ninja"
      [ testAesonSC 2   (Ty.Proxy @IR.Ninja)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Pool"
      [ testAesonSC def (Ty.Proxy @IR.Pool)
      , testAesonSC def (Ty.Proxy @IR.PoolName)
      , testAesonSC def (Ty.Proxy @IR.PoolDepth)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Rule"
      [ testAesonSC 1   (Ty.Proxy @IR.Rule)
      , testAesonSC def (Ty.Proxy @IR.SpecialDeps)
      , testAesonSC def (Ty.Proxy @IR.ResponseFile)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Target"
      [ testAesonSC def (Ty.Proxy @IR.Target)
      , testAesonSC def (Ty.Proxy @IR.Output)
      , testAesonSC def (Ty.Proxy @IR.Dependency)
      , testAesonSC def (Ty.Proxy @IR.DependencyType)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.AST.Env"
      [ testAesonSC def (Ty.Proxy @(AST.Env Text Text))
      , testAesonQC     (Ty.Proxy @(AST.Env Text Text))
      ]
    , testModule "Language.Ninja.AST.Expr"
      [ testAesonSC def (Ty.Proxy @(AST.Expr Bool))
      , testAesonQC     (Ty.Proxy @(AST.Expr Bool))
      ]
    , testModule "Language.Ninja.AST.Rule"
      [ testAesonSC def (Ty.Proxy @(AST.Rule Bool))
      , testAesonQC     (Ty.Proxy @(AST.Rule Bool))
      ]
    , testModule "Language.Ninja.AST.Ninja"
      [ -- TODO: combinatorial explosion
        testAesonSC 0   (Ty.Proxy @(AST.Ninja Bool))
      -- , testAesonQC     (Ty.Proxy @(AST.Ninja Bool))
      ]
    , testModule "Language.Ninja.AST.Build"
      [ -- TODO: combinatorial explosion
        testAesonSC 1   (Ty.Proxy @(AST.Build Bool))
      -- , testAesonQC     (Ty.Proxy @(AST.Build Bool))
      ]
    , testModule "Language.Ninja.AST.Deps"
      [ testAesonSC def (Ty.Proxy @(AST.Deps Bool))
      , testAesonQC     (Ty.Proxy @(AST.Deps Bool))
      ]
    , testModule "Language.Ninja.Misc.Command"
      [ testAesonSC def (Ty.Proxy @Misc.Command)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.Path"
      [ testAesonSC def (Ty.Proxy @Misc.Path)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.Located"
      [ testAesonSC def (Ty.Proxy @(Misc.Located Bool))
      , testAesonSC def (Ty.Proxy @Misc.Position)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.IText"
      [ testAesonSC def (Ty.Proxy @Misc.IText)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    ]
  where
    testAesonSC :: forall x.
                   ( Eq x, Show x, Ty.Typeable x, SC.Serial IO x
                   , Aeson.ToJSON x, Aeson.FromJSON x
                   ) => Int -> Ty.Proxy x -> Maybe TestTree
    testAesonSC d p = withDepth d
                      (testType p
                       [Test.testGroup "ToJSON/FromJSON Laws" [aesonSC p]])

    testAesonQC :: forall x.
                   ( Eq x, Show x, Ty.Typeable x, QC.Arbitrary x
                   , Aeson.ToJSON x, Aeson.FromJSON x
                   ) => Ty.Proxy x -> Maybe TestTree
    testAesonQC p = Just (testType p
                          [Test.testGroup "ToJSON/FromJSON Laws" [aesonQC p]])

    withDepth :: Int -> (TestTree -> Maybe TestTree)
    withDepth 0     = const Nothing
    withDepth depth = Test.localOption (Test.SC.SmallCheckDepth depth) .> Just

    testModule :: TestName -> [Maybe TestTree] -> TestTree
    testModule name subtrees = Test.testGroup name (catMaybes subtrees)

    testType :: forall x. (Ty.Typeable x)
             => Ty.Proxy x -> [TestTree] -> TestTree
    testType p subtrees = Test.localOption typeTimeout
                          $ Test.testGroup (printProxy p) subtrees

    printProxy :: forall x. (Ty.Typeable x) => Ty.Proxy x -> String
    printProxy p = Ty.showsTypeRep (Ty.typeRep p) ""

    def :: Int
    def = (Test.defaultValue :: Test.SC.SmallCheckDepth)
          |> toInteger |> fromIntegral

    typeTimeout :: Test.Timeout
    typeTimeout = Test.mkTimeout 20000000 -- 20 seconds

opticsTests :: TestTree
opticsTests
  = Test.testGroup "optics"
    [ testModule "Language.Ninja.IR.Build"
      [ testType "Build" [] -- TODO: combinatorial explosion
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
      [ testType "Ninja" [] -- TODO: combinatorial explosion
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
      [ testType "Rule" [] -- TODO: combinatorial explosion
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
        ]
      , testType "Dependency"
        [ testLens def "dependencyTarget" IR.dependencyTarget
        , testLens def "dependencyType"   IR.dependencyType
        ]
      , testType "DependencyType"
        [ testPrism def "_NormalDependency"    IR._NormalDependency
        , testPrism def "_OrderOnlyDependency" IR._OrderOnlyDependency
        ]
      ]
    , testModule "Language.Ninja.AST.Env"
      [ testType "Env"
        [ testIso 1 "fromEnv"
          (AST.fromEnv :: Lens.Iso' (AST.Env Text Int) (AST.Maps Text Int))
        ]
      ]
    , testModule "Language.Ninja.AST.Expr"
      [ testType "Expr"
        [ testPrism 4 "_Exprs" (AST._Exprs @Bool)
        , testPrism 4 "_Lit"   (AST._Lit   @Bool)
        , testPrism 4 "_Var"   (AST._Var   @Bool)
        ]
      ]
    , testModule "Language.Ninja.AST.Rule"
      [ testType "Rule"
        [ testLens 4 "ruleBind" (AST.ruleBind @Bool)
        ]
      ]
    , testModule "Language.Ninja.AST.Ninja"
      [ testType "Ninja" [] -- TODO: combinatorial explosion
        -- [ testLens 1 "ninjaRules"     (AST.ninjaRules     @Bool)
        -- , testLens 1 "ninjaSingles"   (AST.ninjaSingles   @Bool)
        -- , testLens 1 "ninjaMultiples" (AST.ninjaMultiples @Bool)
        -- , testLens 1 "ninjaPhonys"    (AST.ninjaPhonys    @Bool)
        -- , testLens 1 "ninjaDefaults"  (AST.ninjaDefaults  @Bool)
        -- , testLens 1 "ninjaSpecials"  (AST.ninjaSpecials  @Bool)
        -- ]
      ]
    , testModule "Language.Ninja.AST.Build"
      [ testType "Build" [] -- TODO: combinatorial explosion
        -- [ testLens 1 "buildRule" (AST.buildRule @Bool)
        -- , testLens 1 "buildEnv"  (AST.buildEnv  @Bool)
        -- , testLens 1 "buildDeps" (AST.buildDeps @Bool)
        -- , testLens 1 "buildBind" (AST.buildBind @Bool)
        -- ]
      ]
    , testModule "Language.Ninja.AST.Deps"
      [ testType "Deps"
        [ testLens def "depsNormal"    (AST.depsNormal    @Bool)
        , testLens def "depsImplicit"  (AST.depsImplicit  @Bool)
        , testLens def "depsOrderOnly" (AST.depsOrderOnly @Bool)
        ]
      ]
    , testModule "Language.Ninja.Misc.Command"
      [ testType "Command"
        [ testIso def "commandText" Misc.commandText
        ]
      ]
    , testModule "Language.Ninja.Misc.Path"
      [ testType "Path"
        [ testIso def "pathIText"  Misc.pathIText
        , testIso def "pathText"   Misc.pathText
        , testIso def "pathString" Misc.pathString
        , testIso def "pathFP"     Misc.pathFP
        ]
      ]
    , testModule "Language.Ninja.Misc.Located"
      [ testType "Located"
        [ testLens 4 "locatedPos"
          (Misc.locatedPos :: Lens.Lens' (Misc.Located Bool) Misc.Position)
        , testLens 4 "locatedVal"
          (Misc.locatedVal :: Lens.Lens' (Misc.Located Bool) Bool)
        ]
      , testType "Position"
        [ testLens 4 "positionFile" Misc.positionFile
        , testLens 4 "positionLine" Misc.positionLine
        , testLens 4 "positionCol"  Misc.positionCol
        ]
      ]
    , testModule "Language.Ninja.Misc.IText"
      [ testType "IText"
        [ testIso def "itext" Misc.itext
        ]
      ]
    ]
  where
    testIso   :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial Identity s, SC.Serial IO s, SC.CoSerial IO s
                 , SC.Serial Identity a, SC.Serial IO a, SC.CoSerial IO a
                 ) => Int -> TestName -> Lens.Iso' s a -> TestTree
    testLens  :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial IO s, SC.Serial IO a
                 , SC.Serial Identity a, SC.CoSerial IO a
                 ) => Int -> TestName -> Lens.Lens' s a -> TestTree
    testPrism :: ( Eq s, Eq a, Show s, Show a
                 , SC.Serial IO s, SC.Serial IO a
                 , SC.Serial Identity a, SC.CoSerial IO a
                 ) => Int -> TestName -> Lens.Prism' s a -> TestTree
    testIso   d name i = withDepth d $ Test.testGroup name [Test.Iso.test   i]
    testLens  d name l = withDepth d $ Test.testGroup name [Test.Lens.test  l]
    testPrism d name p = withDepth d $ Test.testGroup name [Test.Prism.test p]

    withDepth :: Int -> (TestTree -> TestTree)
    withDepth depth = Test.localOption (Test.SC.SmallCheckDepth depth)

    testModule :: TestName -> [TestTree] -> TestTree
    testModule = Test.testGroup

    testType :: TestName -> [TestTree] -> TestTree
    testType name subtrees = Test.localOption typeTimeout
                             $ Test.testGroup name subtrees

    def :: Int
    def = (Test.defaultValue :: Test.SC.SmallCheckDepth)
          |> toInteger |> fromIntegral

    typeTimeout :: Test.Timeout
    typeTimeout = Test.mkTimeout 20000000 -- 20 seconds

ingredients :: IO [Test.Ingredient]
ingredients = [ [Test.htmlRunner]
              , Test.defaultIngredients
              ] |> mconcat |> pure

testTree :: IO TestTree
testTree = do
  ninjas <- forM testFiles parseTestNinja

  haddockTests <- Lint.emptyLintHaddockOptions
                  |> Lint.addComponentName "language-ninja"
                  |> Lint.lintHaddock

  let tests = Test.testGroup "language-ninja"
              [ Test.testGroup "golden"
                 (fmap (uncurry ninjaTests) (zip testFiles ninjas))
              , aesonTests
              , opticsTests
              , haddockTests
              ]
  pure tests

test :: IO ()
test = do
  is <- ingredients
  tree <- testTree
  Test.defaultMainWithIngredients is tree

main :: IO ()
main = do
  test

--------------------------------------------------------------------------------
