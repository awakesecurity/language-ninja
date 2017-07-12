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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}

-- |
--   Module      : Tests.Lint
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
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
import           Data.Maybe                  (catMaybes, isJust, mapMaybe)
import           Data.Monoid                 ((<>))

import           Flow                        ((.>), (|>))

import qualified GHC
import qualified Outputable                  as GHC

import qualified Data.Map.Lazy               as Map

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

-- | FIXME: doc
lintHaddock :: LintHaddockOptions -> IO Test.TestTree
lintHaddock options = do
  _ <- H.withGhc [] GHC.getSessionDynFlags
  buildHaddock
  mapM (lintInterfaceFile options) (componentNames options)
    |> fmap (Test.testGroup "Haddock linting")

--------------------------------------------------------------------------------

-- | FIXME: doc
data LintHaddockOptions
  = MkLintHaddockOptions
    { componentNames   :: [ComponentName]
    , sinceExceptions  :: [SinceException]
    , haddockArguments :: [String]
    }
  deriving (Eq, Show)

-- | FIXME: doc
emptyLintHaddockOptions :: LintHaddockOptions
emptyLintHaddockOptions = MkLintHaddockOptions [] [] []

-- | FIXME: doc
addComponentName :: ComponentName
                 -> (LintHaddockOptions -> LintHaddockOptions)
addComponentName cn = modifyComponentNames (cn :)

-- | FIXME: doc
addSinceException :: SinceException
                  -> (LintHaddockOptions -> LintHaddockOptions)
addSinceException se = modifySinceExceptions (se :)

-- | FIXME: doc
addHaddockArgument :: String
                   -> (LintHaddockOptions -> LintHaddockOptions)
addHaddockArgument arg = modifyHaddockArguments (arg :)

-- | FIXME: doc
modifySinceExceptions :: ([SinceException] -> [SinceException])
                      -> (LintHaddockOptions -> LintHaddockOptions)
modifySinceExceptions f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { sinceExceptions = f sinceExceptions, .. }

-- | FIXME: doc
modifyComponentNames :: ([ComponentName] -> [ComponentName])
                     -> (LintHaddockOptions -> LintHaddockOptions)
modifyComponentNames f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { componentNames = f componentNames, .. }

-- | FIXME: doc
modifyHaddockArguments :: ([String] -> [String])
                       -> (LintHaddockOptions -> LintHaddockOptions)
modifyHaddockArguments f (MkLintHaddockOptions {..})
  = MkLintHaddockOptions { haddockArguments = f haddockArguments, .. }

--------------------------------------------------------------------------------

-- | FIXME: doc
type ComponentName = String

-- | FIXME: doc
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
      Test.testGroup ifaceName (mapMaybe (checkSince options) xs)

checkSince :: LintHaddockOptions -> (Ident, H.MDoc Ident) -> Maybe Test.TestTree
checkSince options (ident, mdoc)
  = if qname `elem` sinceExceptions options
    then Nothing
    else Just (Test.testCase name
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
                   Map.lookup name docmap
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
