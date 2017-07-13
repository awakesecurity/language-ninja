-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests/Lint.hs
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
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

-- |
--   Module      : Tests.Lint
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Tests that involve linting the project.
--   In particular, Haddock linting is currently supported.
module Tests.Lint
  ( lintHaddock

  , LintHaddockOptions
  , emptyLintHaddockOptions
  , addComponentName, addSinceException, addHaddockArgument
  , modifyComponentNames, modifySinceExceptions, modifyHaddockArguments

  , ComponentName, SinceException
  ) where

import           Control.Applicative         (empty)
import           Control.Arrow               ((&&&))
import           Control.Monad               (forM)

import           Data.Functor.Identity       (runIdentity)
import           Data.Maybe                  (catMaybes, isJust)
import           Data.Monoid                 ((<>))

import           Flow                        ((.>), (|>))

import qualified GHC
import qualified Outputable                  as GHC

import qualified Data.Map.Lazy               as LMap

import qualified Data.Text                   as Text

import qualified Documentation.Haddock       as H
import qualified Documentation.Haddock.Types as H

-- import qualified Documentation.Haddock.Doc          as H.Doc
-- import qualified Documentation.Haddock.Parser       as H.Parser
-- import qualified Documentation.Haddock.Parser.Monad as H.Parser

import qualified Test.Tasty                  as Test
import qualified Test.Tasty.HUnit            as Test

import qualified Turtle

--------------------------------------------------------------------------------

-- | Generate and lint the Haddock documentation for this project.
lintHaddock :: LintHaddockOptions -> IO Test.TestTree
lintHaddock options = do
  _ <- H.withGhc [] GHC.getSessionDynFlags
  buildHaddock
  mapM (lintInterfaceFile options) (componentNames options)
    |> fmap (Test.testGroup "Haddock linting")

--------------------------------------------------------------------------------

-- | A type for options provided to 'lintHaddock'.
data LintHaddockOptions
  = MkLintHaddockOptions
    { componentNames   :: [ComponentName]
    , sinceExceptions  :: [SinceException]
    , haddockArguments :: [String]
    }
  deriving (Eq, Show)

-- | An empty 'LintHaddockOptions'.
emptyLintHaddockOptions :: LintHaddockOptions
emptyLintHaddockOptions = MkLintHaddockOptions [] [] []

-- | Add the given 'ComponentName' to the given 'LintHaddockOptions'.
addComponentName :: ComponentName
                 -> (LintHaddockOptions -> LintHaddockOptions)
addComponentName cn = modifyComponentNames (cn :)

-- | Add the given 'SinceException' to the given 'LintHaddockOptions'.
addSinceException :: SinceException
                  -> (LintHaddockOptions -> LintHaddockOptions)
addSinceException se = modifySinceExceptions (se :)

-- | Add the given Haddock argument to the given 'LintHaddockOptions'.
addHaddockArgument :: String
                   -> (LintHaddockOptions -> LintHaddockOptions)
addHaddockArgument arg = modifyHaddockArguments (arg :)

-- | Modify the list of 'ComponentName's with the given function.
modifyComponentNames :: ([ComponentName] -> [ComponentName])
                     -> (LintHaddockOptions -> LintHaddockOptions)
modifyComponentNames f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { componentNames = f componentNames, .. }

-- | Modify the list of 'SinceException's with the given function.
modifySinceExceptions :: ([SinceException] -> [SinceException])
                      -> (LintHaddockOptions -> LintHaddockOptions)
modifySinceExceptions f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { sinceExceptions = f sinceExceptions, .. }

-- | Modify the list of Haddock arguments with the given function.
modifyHaddockArguments :: ([String] -> [String])
                       -> (LintHaddockOptions -> LintHaddockOptions)
modifyHaddockArguments f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { haddockArguments = f haddockArguments, .. }

--------------------------------------------------------------------------------

-- | The name of a Cabal component for which the Haddock documentation should
--   be linted.
type ComponentName = String

-- | An exception to the checker that ensures that every top-level exposed
--   declaration has an @\@since@ attribute; this takes the form of a
--   fully-qualified Haskell name, e.g.: @Control.Monad.unless@.
type SinceException = String

--------------------------------------------------------------------------------

lintInterfaceFile :: LintHaddockOptions -> ComponentName -> IO Test.TestTree
lintInterfaceFile options cn
  = readInterface ifaceFP
    >>= snd
    .>  map ((id &&& deconstruct) .> uncurry helper)
    .>  Test.testGroup cn
    .>  pure
  where
    ifaceFP :: FilePath
    ifaceFP = "./dist/doc/html/" <> cn <> "/" <> cn <> ".haddock"

    helper :: H.InstalledInterface -> [(Ident, H.MDoc Ident)] -> Test.TestTree
    helper iface xs = do
      let ifaceName = H.instMod iface
                      |> GHC.moduleName
                      |> GHC.moduleNameString
      Test.testGroup ifaceName (map (checkSince options) xs)

checkSince :: LintHaddockOptions -> (Ident, H.MDoc Ident) -> Test.TestTree
checkSince options (ident, mdoc)
  = if qname `elem` sinceExceptions options
    then (Test.testCaseInfo name (pure (qname <> " is in exceptions list")))
    else (Test.testCase name
          (Test.assertBool message (hasSince mdoc)))
  where
    name = printIdentName ident
    qname = printIdent ident
    message = qname <> " does not have @since"

readInterface :: FilePath -> IO (H.LinkEnv, [H.InstalledInterface])
readInterface fp = H.readInterfaceFile H.freshNameCache fp
                   >>= either fail (decomposeIF .> pure)
  where
    decomposeIF = H.ifLinkEnv &&& H.ifInstalledIfaces

buildHaddock :: IO ()
buildHaddock = runHaddock []
  where
    runHaddock :: [String] -> IO ()
    runHaddock extraArgs = Turtle.sh $ do
      let args = ["./Setup.hs", "haddock"] ++ map Text.pack extraArgs
      Turtle.inprocWithErr "runhaskell" args empty

deconstruct :: H.InstalledInterface -> [(Ident, H.MDoc Ident)]
deconstruct iface
  = (catMaybes $ runIdentity $ forM exports
     $ \name -> do let ident = toIdent name
                   LMap.lookup name docmap
                     |> fmap (\doc -> (ident, toIdent <$> doc))
                     |> pure)
    -- maybe (fail ("could not find: " <> printIdent ident)) pure
    -- pure (ident, toIdent <$> doc)
  where
    exports = H.instVisibleExports iface
    docmap  = H.instDocMap iface
    toIdent = MkIdent (H.instMod iface)

--------------------------------------------------------------------------------

data Ident
  = MkIdent
    { _identModule :: !GHC.Module
    , _identName   :: !GHC.Name
    }
  deriving ()

printIdentModule :: Ident -> String
printIdentModule (MkIdent {..})
  = GHC.moduleNameString (GHC.moduleName _identModule)

-- NOTE: this fails if the GHC dynflags have not been set up
printIdentName :: Ident -> String
printIdentName (MkIdent {..})
  = GHC.showSDocUnsafe (GHC.ppr _identName)

printIdent :: Ident -> String
printIdent ident = printIdentModule ident <> "." <> printIdentName ident

instance Show Ident where
  show = printIdent

--------------------------------------------------------------------------------

class HasSince t where
  hasSince :: t -> Bool

instance HasSince (H.MDoc name) where
  hasSince = H._meta .> H._version .> isJust

instance HasSince (H.Documentation name) where
  hasSince = H.documentationDoc .> maybe False hasSince

--------------------------------------------------------------------------------
