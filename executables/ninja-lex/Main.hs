-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/ninja-lex/Main.hs
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
import           Control.Monad.Except     (runExcept)

import           Options.Generic          ((:::), type (<?>))
import qualified Options.Generic          as Opt

import           GHC.Generics             (Generic)

import           Data.Char                (toLower)
import           Data.Functor             (void)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))

import           Data.Version             (showVersion)
import qualified Paths_language_ninja     as Paths

import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text

import qualified Data.Text.Lazy           as LText
import qualified Data.Text.Lazy.Encoding  as LText
import qualified Data.Text.Lazy.IO        as LText

import qualified Language.Ninja.Errors    as Errors
import qualified Language.Ninja.Lexer     as Lexer
import           Language.Ninja.Misc.Path (Path, pathFP, pathString)
import qualified Language.Ninja.Pretty    as Pretty

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import qualified System.IO                as IO

import           Flow                     ((.>))

--------------------------------------------------------------------------------

version :: String
version = showVersion Paths.version

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

type OptionProcessInput
  = Maybe Path <?> "Read the given FILEPATH as a Ninja file."
type OptionProcessOutput
  = Maybe Path <?> "Output to the given FILEPATH instead of /dev/stdout."
type OptionProcessMachineReadable
  = Bool       <?> "Should the output be fully machine-readable?"

type OptionPrettyInput
  = Maybe Path <?> "Read the given FILEPATH as a Ninja file."
type OptionPrettyOutput
  = Maybe Path <?> "Output to the given FILEPATH instead of /dev/stdout."
type OptionPrettyColor
  = Bool       <?> "Should the output use ANSI color?"

data Options' w
  = Process { _optionsInput           :: (w ::: OptionProcessInput)
            , _optionsOutput          :: (w ::: OptionProcessOutput)
            , _optionsMachineReadable :: (w ::: OptionProcessMachineReadable) }
  | Pretty  { _optionsInput  :: (w ::: OptionPrettyInput)
            , _optionsOutput :: (w ::: OptionPrettyOutput)
            , _optionsColor  :: (w ::: OptionPrettyColor) }
  deriving (Generic)

instance Opt.ParseRecord (Options' Opt.Wrapped) where
  parseRecord = Opt.parseRecordWithModifiers
                (composeModifiers
                 (removePrefixModifier "_options")
                 Opt.lispCaseModifiers)

deriving instance Show (Options' Opt.Unwrapped)

type Options = Options' Opt.Unwrapped

parseOptions :: IO Options
parseOptions = Opt.unwrapRecord (Text.pack ("ninja-lex version " <> version))

--------------------------------------------------------------------------------

processNinja :: Maybe Path -> (IO.Handle, IO.Handle) -> Bool -> IO ()
processNinja inputPath (input, output) machineReadable = do
  let encodeJ :: Aeson.Value -> Text
      encodeJ = (if machineReadable then Aeson.encode else Aeson.encodePretty)
                .> LText.decodeUtf8 .> LText.toStrict

  let throwMR :: Errors.ParseError -> IO ()
      throwMR = Aeson.toJSON .> encodeJ .> Text.hPutStrLn output

  inputText <- Text.hGetContents input

  case runExcept (Lexer.lexTextWithPath inputPath inputText) of
    Left  e -> if machineReadable
               then throwMR e
               else throwIO e
    Right x -> Text.hPutStr output (encodeJ (Aeson.toJSON x))

prettyNinja :: Maybe Path -> (IO.Handle, IO.Handle) -> Bool -> IO ()
prettyNinja _inputPath (input, output) _color = do
  -- TODO: add color support
  inputText <- LText.hGetContents input
  let parsed :: Either String [Lexer.Lexeme Lexer.Ann]
      parsed = Aeson.eitherDecode (LText.encodeUtf8 inputText)
  case parsed of
    Left  e -> IO.hPutStrLn IO.stderr e
    Right x -> Text.hPutStr output (Pretty.prettyLexemes (map void x))

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
    (Process i o mr) -> pairH i o >>= \p -> processNinja i p mr >> closeH p
    (Pretty  i o c)  -> pairH i o >>= \p -> prettyNinja  i p c  >> closeH p

--------------------------------------------------------------------------------

main :: IO ()
main = do
  parseOptions >>= runOptions

--------------------------------------------------------------------------------
