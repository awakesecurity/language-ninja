-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Rule.hs
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Language.Ninja.AST.Rule
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Ninja @rule@ declarations.
module Language.Ninja.AST.Rule
  ( -- * @Rule@
    Rule, makeRule
  , ruleName, ruleCommand, ruleDescription, rulePool, ruleDepfile
  , ruleSpecialDeps, ruleGenerator, ruleRestat, ruleResponseFile

    -- * @SpecialDeps@
  , SpecialDeps, makeSpecialDepsGCC, makeSpecialDepsMSVC
  , _SpecialDepsGCC, _SpecialDepsMSVC

    -- * @ResponseFile@
  , ResponseFile, makeResponseFile, responseFilePath, responseFileContent
  ) where

import           Language.Ninja.AST.Pool     (PoolName, poolNameDefault)
import           Language.Ninja.Misc.Command (Command)
import           Language.Ninja.Misc.Path    (Path)

import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Data.Aeson                  as Aeson

import           Data.Hashable               (Hashable (..))
import           GHC.Generics                (Generic)

import           Control.Lens.Lens           (Lens', lens)
import           Control.Lens.Prism          (Prism', prism)

import           Flow                        ((|>))

--------------------------------------------------------------------------------

-- | A Ninja @rule@ declaration, as documented
--   <https://ninja-build.org/manual.html#_rules here>.
data Rule
  = MkRule
    { _ruleName         :: !Text
    , _ruleCommand      :: !Command
    , _ruleDescription  :: !(Maybe Text)
    , _rulePool         :: !PoolName
    , _ruleDepfile      :: !(Maybe Path)
    , _ruleSpecialDeps  :: !(Maybe SpecialDeps)
    , _ruleGenerator    :: !Bool
    , _ruleRestat       :: !Bool
    , _ruleResponseFile :: !(Maybe ResponseFile)
    }
  deriving (Eq, Ord, Show, Generic)

-- | Construct an 'Rule' with the given name and command, with default values
--   for all other attributes (e.g.: 'False', 'Nothing', 'poolDefault').
makeRule :: Text
         -- ^ The rule name.
         -> Command
         -- ^ The command to run.
         -> Rule
         -- ^ A rule that runs this command.
makeRule name cmd
  = MkRule
    { _ruleName         = name
    , _ruleCommand      = cmd
    , _ruleDescription  = Nothing
    , _rulePool         = poolNameDefault
    , _ruleDepfile      = Nothing
    , _ruleSpecialDeps  = Nothing
    , _ruleGenerator    = False
    , _ruleRestat       = False
    , _ruleResponseFile = Nothing
    }

-- | The name of the rule.
ruleName :: Lens' Rule Text
ruleName = lens _ruleName
           $ \(MkRule {..}) x -> MkRule { _ruleName = x, .. }

-- | The command that this rule will run.
ruleCommand :: Lens' Rule Command
ruleCommand = lens _ruleCommand
              $ \(MkRule {..}) x -> MkRule { _ruleCommand = x, .. }

-- | A short description of the command, used to pretty-print the command
--   as it's running. The @ninja -v@ flag controls whether to print the
--   full command or its description; if a command fails, the full command
--   line will always be printed before the command's output.
ruleDescription :: Lens' Rule (Maybe Text)
ruleDescription = lens _ruleDescription
                  $ \(MkRule {..}) x -> MkRule { _ruleDescription = x, .. }

-- | The process pool in which this rule will be executed.
rulePool :: Lens' Rule PoolName
rulePool = lens _rulePool
           $ \(MkRule {..}) x -> MkRule { _rulePool = x, .. }

-- | If set, this should be a path to an optional Makefile that contains
--   extra implicit dependencies. This is used to support C/C++ header
--   dependencies. For more information, read the Ninja documentation
--   <https://ninja-build.org/manual.html#_depfile here>.
ruleDepfile :: Lens' Rule (Maybe Path)
ruleDepfile = lens _ruleDepfile
              $ \(MkRule {..}) x -> MkRule { _ruleDepfile = x, .. }

-- | If set, enables special dependency processing used in C/C++ header
--   dependencies. For more information, read the Ninja documentation
--   <https://ninja-build.org/manual.html#_deps here>.
ruleSpecialDeps :: Lens' Rule (Maybe SpecialDeps)
ruleSpecialDeps = lens _ruleSpecialDeps
                  $ \(MkRule {..}) x -> MkRule { _ruleSpecialDeps = x, .. }

-- | If this is true, specifies that this rule is used to re-invoke the
--   generator program. Files built using generator rules are treated
--   specially in two ways: firstly, they will not be rebuilt if the
--   command line changes; and secondly, they are not cleaned by default.
ruleGenerator :: Lens' Rule Bool
ruleGenerator = lens _ruleGenerator
                $ \(MkRule {..}) x -> MkRule { _ruleGenerator = x, .. }

-- | If true, causes Ninja to re-stat the command's outputs after
--   execution of the command. Each output whose modification time the
--   command did not change will be treated as though it had never needed
--   to be built. This may cause the output's reverse dependencies to be
--   removed from the list of pending build actions.
ruleRestat :: Lens' Rule Bool
ruleRestat = lens _ruleRestat
             $ \(MkRule {..}) x -> MkRule { _ruleRestat = x, .. }

-- | If present, Ninja will use a response file for the given command,
--   i.e. write the selected string to the given file before calling the
--   command and delete the file after the command is done.
--
--   This is particularly useful on Windows OS, where the maximal length
--   of a command line is limited and response files must be used instead.
ruleResponseFile :: Lens' Rule (Maybe ResponseFile)
ruleResponseFile = lens _ruleResponseFile
                   $ \(MkRule {..}) x -> MkRule { _ruleResponseFile = x, .. }

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Rule

-- | Converts to
--   @{name: …, command: …, desc: …, pool: …, depfile: …,
--     deps: …, generator: …, restat: …, rsp: …}@.
instance ToJSON Rule where
  toJSON (MkRule {..})
    = [ "name"      .= _ruleName
      , "command"   .= _ruleCommand
      , "desc"      .= _ruleDescription
      , "pool"      .= _rulePool
      , "depfile"   .= _ruleDepfile
      , "deps"      .= _ruleSpecialDeps
      , "generator" .= _ruleGenerator
      , "restat"    .= _ruleRestat
      , "rsp"       .= _ruleResponseFile
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Rule where
  parseJSON = (withObject "Rule" $ \o -> do
                  _ruleName         <- (o .: "name")      >>= pure
                  _ruleCommand      <- (o .: "command")   >>= pure
                  _ruleDescription  <- (o .: "desc")      >>= pure
                  _rulePool         <- (o .: "pool")      >>= pure
                  _ruleDepfile      <- (o .: "depfile")   >>= pure
                  _ruleSpecialDeps  <- (o .: "deps")      >>= pure
                  _ruleGenerator    <- (o .: "generator") >>= pure
                  _ruleRestat       <- (o .: "restat")    >>= pure
                  _ruleResponseFile <- (o .: "rsp")       >>= pure
                  pure (MkRule {..}))

--------------------------------------------------------------------------------

-- | Special dependency information, as described
--   <https://ninja-build.org/manual.html#ref_headers here>.
data SpecialDeps
  = SpecialDepsGCC
  | SpecialDepsMSVC !(Maybe Text)
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct a 'SpecialDeps' corresponding to the case in which @deps = gcc@
--   is set in a Ninja build rule.
makeSpecialDepsGCC :: SpecialDeps
makeSpecialDepsGCC = SpecialDepsGCC

-- | Construct a 'SpecialDeps' corresponding to the case in which @deps = msvc@
--   is set and @msvc_deps_prefix = …@.
--
--   The @msvc_deps_prefix@ field defines the string which should be stripped
--   from @msvc@'s @/showIncludes@ output. It is only needed if the version of
--   Visual Studio being used is not English.
makeSpecialDepsMSVC :: Maybe Text
                    -- ^ If this is @Just …@, set @msvc_deps_prefix@ to the
                    --   given text; otherwise it will not be set.
                    -> SpecialDeps
makeSpecialDepsMSVC = SpecialDepsMSVC

-- | A prism for the @deps = gcc@ case.
_SpecialDepsGCC :: Prism' SpecialDeps ()
_SpecialDepsGCC = prism (const SpecialDepsGCC)
                  $ \case SpecialDepsGCC -> Right ()
                          owise          -> Left owise

-- | A prism for the @deps = msvc@ / @msvc_deps_prefix = …@ case.
_SpecialDepsMSVC :: Prism' SpecialDeps (Maybe Text)
_SpecialDepsMSVC = prism SpecialDepsMSVC
                   $ \case (SpecialDepsMSVC prefix) -> Right prefix
                           owise                    -> Left owise

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable SpecialDeps

-- | Converts to @{deps: "gcc"}@ or @{deps: "msvc", prefix: …}@.
instance ToJSON SpecialDeps where
  toJSON = go
    where
      go SpecialDepsGCC             = object ["deps" .= gcc]
      go (SpecialDepsMSVC Nothing)  = object ["deps" .= msvc]
      go (SpecialDepsMSVC (Just p)) = object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Value
      (gcc, msvc) = ("gcc", "msvc")

-- | Inverse of the 'ToJSON' instance.
instance FromJSON SpecialDeps where
  parseJSON = withObject "SpecialDeps" $ \o -> do
    deps <- o .: "deps"
    prefix <- o .:? "prefix"
    case T.pack deps of
      "gcc"  -> pure SpecialDepsGCC
      "msvc" -> pure (SpecialDepsMSVC prefix)
      owise  -> ["Invalid deps type ", "\"", owise, "\"; "
                , "should be one of [\"gcc\", \"msvc\"]."
                ] |> mconcat |> T.unpack |> fail

--------------------------------------------------------------------------------

-- | A response file to use during rule execution, as documented
--   <https://ninja-build.org/manual.html#ref_rule here>.
data ResponseFile
  = MkResponseFile
    { _responseFilePath    :: !Path
    , _responseFileContent :: !Text
    }
  deriving (Eq, Ord, Show, Generic)

-- | Construct a 'ResponseFile' with the given 'Path' and content 'Text'.
makeResponseFile :: Path
                 -- ^ Corresponds to @rspfile@.
                 -> Text
                 -- ^ Corresponds to @rspfile_content@.
                 -> ResponseFile
makeResponseFile = MkResponseFile

-- | A lens for the @rspfile@ field.
responseFilePath :: Lens' ResponseFile Path
responseFilePath = lens _responseFilePath
                   $ \(MkResponseFile {..}) x ->
                       MkResponseFile { _responseFilePath = x, .. }

-- | A lens for the @rspfile_content@ field.
responseFileContent :: Lens' ResponseFile Text
responseFileContent = lens _responseFileContent
                      $ \(MkResponseFile {..}) x ->
                          MkResponseFile { _responseFileContent = x, .. }

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable ResponseFile

-- | Converts to @{path: …, content: …}@.
instance ToJSON ResponseFile where
  toJSON (MkResponseFile {..})
    = [ "path"    .= _responseFilePath
      , "content" .= _responseFileContent
      ] |> object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON ResponseFile where
  parseJSON = withObject "ResponseFile" $ \o -> do
    MkResponseFile
      <$> (o .: "path")
      <*> (o .: "content")

--------------------------------------------------------------------------------
