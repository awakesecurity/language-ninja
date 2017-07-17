-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/ninja-compile/Main.hs
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTSyntax         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
module Main (main) where

import qualified Control.Lens             as Lens

import           Control.Exception        (throwIO)
import           Control.Monad.Except     (runExcept, runExceptT)

import           Options.Generic          ((:::), type (<?>))
import qualified Options.Generic          as Opt

import           GHC.Generics             (Generic)

import           Data.Char                (toLower)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))

import           Data.Version             (showVersion)
import qualified Paths_language_ninja     as Paths

import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text

import qualified Data.Text.Lazy           as LText
import qualified Data.Text.Lazy.Encoding  as LText

import qualified Language.Ninja.Compile   as Compile
import qualified Language.Ninja.Errors    as Errors
import           Language.Ninja.Misc.Path (Path, pathFP, pathString)
import qualified Language.Ninja.Parser    as Parser

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import qualified System.IO                as IO

import           Flow                     ((.>))

--------------------------------------------------------------------------------

version :: Text
version = Text.pack (showVersion Paths.version)

--------------------------------------------------------------------------------

instance Opt.ParseField Path where
  parseField h m = Lens.view (Lens.from pathFP) <$> Opt.parseField h m

composeModifiers :: Opt.Modifiers -> Opt.Modifiers -> Opt.Modifiers
composeModifiers (Opt.Modifiers fnX cnX) (Opt.Modifiers fnY cnY)
  = Opt.Modifiers (fnX .> fnY) (cnX .> cnY)

removePrefixModifier :: Text -> Opt.Modifiers
removePrefixModifier prefix = Opt.Modifiers removeFN id
  where
    removeFN :: String -> String
    removeFN str = fromMaybe str $ do
      stripped <- Text.stripPrefix prefix (Text.pack str)
      (c, rest) <- Text.uncons stripped
      pure (toLower c : Text.unpack rest)

--------------------------------------------------------------------------------

type OptionInput
  = Maybe Path <?> "Read the given FILEPATH as a Ninja file."
type OptionOutput
  = Maybe Path <?> "Output to the given FILEPATH instead of /dev/stdout."
type OptionMachineReadable
  = Bool       <?> "Should the output be fully machine-readable?"

data Options' w
  = Options'
    { _optionsInput           :: (w ::: OptionInput)
    , _optionsOutput          :: (w ::: OptionOutput)
    , _optionsMachineReadable :: (w ::: OptionMachineReadable) }
  deriving (Generic)

instance Opt.ParseRecord (Options' Opt.Wrapped) where
  parseRecord = Opt.parseRecordWithModifiers
                (composeModifiers
                 (removePrefixModifier "_options")
                 Opt.lispCaseModifiers)

deriving instance Show (Options' Opt.Unwrapped)

type Options = Options' Opt.Unwrapped

parseOptions :: IO Options
parseOptions = Opt.unwrapRecord ("ninja-compile version " <> version)

--------------------------------------------------------------------------------

processNinja :: Maybe Path -> (IO.Handle, IO.Handle) -> Bool -> IO ()
processNinja inputPath (input, output) machineReadable = do
  let encodeJ :: Aeson.Value -> Text
      encodeJ = (if machineReadable then Aeson.encode else Aeson.encodePretty)
                .> LText.decodeUtf8 .> LText.toStrict

  let throwMRPE :: Errors.ParseError -> IO ()
      throwMRPE = Aeson.toJSON .> encodeJ .> Text.hPutStrLn output

  let throwMRCE :: Errors.CompileError -> IO ()
      throwMRCE = undefined -- FIXME: implement

  inputText <- Text.hGetContents input

  ast <- runExceptT $ case inputPath of
                        Just path -> Parser.parseFile path
                        Nothing   -> Parser.parseText inputText

  case ast of
    Left  e -> if machineReadable
               then throwMRPE e
               else throwIO e
    Right x -> case runExcept (Compile.compile x) of
                 Left  e -> if machineReadable
                            then throwMRCE e
                            else throwIO e
                 Right c -> Text.hPutStr output (encodeJ (Aeson.toJSON c))

runOptions :: Options -> IO ()
runOptions opts = do
  let pathH :: IO.IOMode -> Path -> IO IO.Handle
      pathH mode p = IO.openFile (Lens.view pathString p) mode

  let inputH, outputH :: Maybe Path -> IO IO.Handle
      inputH  = maybe (pure IO.stdin)  (pathH IO.ReadMode)
      outputH = maybe (pure IO.stdout) (pathH IO.WriteMode)

  let pairH :: Maybe Path -> Maybe Path -> IO (IO.Handle, IO.Handle)
      pairH i o = (,) <$> inputH i <*> outputH o

  let closeH :: (IO.Handle, IO.Handle) -> IO ()
      closeH (i, o) = IO.hClose i >> IO.hClose o >> pure ()

  case opts of
    (Options' i o mr) -> pairH i o >>= \p -> processNinja i p mr >> closeH p

--------------------------------------------------------------------------------

main :: IO ()
main = do
  parseOptions >>= runOptions

--------------------------------------------------------------------------------
