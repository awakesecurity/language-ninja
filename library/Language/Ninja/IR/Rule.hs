-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/IR/Rule.hs
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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.IR.Rule
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Ninja @rule@ declarations.
--
--   @since 0.1.0
module Language.Ninja.IR.Rule
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

import qualified Control.Lens                as Lens

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Aeson                  ((.:), (.=))
import qualified Data.Aeson                  as Aeson

import           Control.DeepSeq             (NFData)
import           Data.Hashable               (Hashable (..))
import           GHC.Generics                (Generic)
import qualified Test.SmallCheck.Series      as SC

import           Language.Ninja.IR.Pool      (PoolName, makePoolNameDefault)
import           Language.Ninja.Misc.Command (Command)
import           Language.Ninja.Misc.Path    (Path)

import           Flow                        ((|>))

--------------------------------------------------------------------------------

-- | A Ninja @rule@ declaration, as documented
--   <https://ninja-build.org/manual.html#_rules here>.
--
--   @since 0.1.0
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
--
--   @since 0.1.0
{-# INLINE makeRule #-}
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
    , _rulePool         = makePoolNameDefault
    , _ruleDepfile      = Nothing
    , _ruleSpecialDeps  = Nothing
    , _ruleGenerator    = False
    , _ruleRestat       = False
    , _ruleResponseFile = Nothing
    }

-- | The name of the rule.
--
--   @since 0.1.0
{-# INLINE ruleName #-}
ruleName :: Lens.Lens' Rule Text
ruleName = Lens.lens _ruleName
           $ \(MkRule {..}) x -> MkRule { _ruleName = x, .. }

-- | The command that this rule will run.
--
--   @since 0.1.0
{-# INLINE ruleCommand #-}
ruleCommand :: Lens.Lens' Rule Command
ruleCommand = Lens.lens _ruleCommand
              $ \(MkRule {..}) x -> MkRule { _ruleCommand = x, .. }

-- | A short description of the command, used to pretty-print the command
--   as it's running. The @ninja -v@ flag controls whether to print the
--   full command or its description; if a command fails, the full command
--   line will always be printed before the command's output.
--
--   @since 0.1.0
{-# INLINE ruleDescription #-}
ruleDescription :: Lens.Lens' Rule (Maybe Text)
ruleDescription = Lens.lens _ruleDescription
                  $ \(MkRule {..}) x -> MkRule { _ruleDescription = x, .. }

-- | The process pool in which this rule will be executed.
--
--   @since 0.1.0
{-# INLINE rulePool #-}
rulePool :: Lens.Lens' Rule PoolName
rulePool = Lens.lens _rulePool
           $ \(MkRule {..}) x -> MkRule { _rulePool = x, .. }

-- | If set, this should be a path to an optional Makefile that contains
--   extra implicit dependencies. This is used to support C/C++ header
--   dependencies. For more information, read the Ninja documentation
--   <https://ninja-build.org/manual.html#_depfile here>.
--
--   @since 0.1.0
{-# INLINE ruleDepfile #-}
ruleDepfile :: Lens.Lens' Rule (Maybe Path)
ruleDepfile = Lens.lens _ruleDepfile
              $ \(MkRule {..}) x -> MkRule { _ruleDepfile = x, .. }

-- | If set, enables special dependency processing used in C/C++ header
--   dependencies. For more information, read the Ninja documentation
--   <https://ninja-build.org/manual.html#_deps here>.
--
--   @since 0.1.0
{-# INLINE ruleSpecialDeps #-}
ruleSpecialDeps :: Lens.Lens' Rule (Maybe SpecialDeps)
ruleSpecialDeps = Lens.lens _ruleSpecialDeps
                  $ \(MkRule {..}) x -> MkRule { _ruleSpecialDeps = x, .. }

-- | If this is true, specifies that this rule is used to re-invoke the
--   generator program. Files built using generator rules are treated
--   specially in two ways: firstly, they will not be rebuilt if the
--   command line changes; and secondly, they are not cleaned by default.
--
--   @since 0.1.0
{-# INLINE ruleGenerator #-}
ruleGenerator :: Lens.Lens' Rule Bool
ruleGenerator = Lens.lens _ruleGenerator
                $ \(MkRule {..}) x -> MkRule { _ruleGenerator = x, .. }

-- | If true, causes Ninja to re-stat the command's outputs after
--   execution of the command. Each output whose modification time the
--   command did not change will be treated as though it had never needed
--   to be built. This may cause the output's reverse dependencies to be
--   removed from the list of pending build actions.
--
--   @since 0.1.0
{-# INLINE ruleRestat #-}
ruleRestat :: Lens.Lens' Rule Bool
ruleRestat = Lens.lens _ruleRestat
             $ \(MkRule {..}) x -> MkRule { _ruleRestat = x, .. }

-- | If present, Ninja will use a response file for the given command,
--   i.e. write the selected string to the given file before calling the
--   command and delete the file after the command is done.
--
--   This is particularly useful on Windows OS, where the maximal length
--   of a command line is limited and response files must be used instead.
--
--   @since 0.1.0
{-# INLINE ruleResponseFile #-}
ruleResponseFile :: Lens.Lens' Rule (Maybe ResponseFile)
ruleResponseFile = Lens.lens _ruleResponseFile
                   $ \(MkRule {..}) x -> MkRule { _ruleResponseFile = x, .. }

-- | Converts to
--   @{name: …, command: …, desc: …, pool: …, depfile: …,
--     deps: …, generator: …, restat: …, rsp: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Rule where
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
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Rule where
  parseJSON = (Aeson.withObject "Rule" $ \o -> do
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

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable Rule

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Rule

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m Rule

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m Rule

--------------------------------------------------------------------------------

-- | Special dependency information, as described
--   <https://ninja-build.org/manual.html#ref_headers here>.
--
--   @since 0.1.0
data SpecialDeps
  = SpecialDepsGCC
  | SpecialDepsMSVC !Text
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct a 'SpecialDeps' corresponding to the case in which @deps = gcc@
--   is set in a Ninja build rule.
--
--   @since 0.1.0
{-# INLINE makeSpecialDepsGCC #-}
makeSpecialDepsGCC :: SpecialDeps
makeSpecialDepsGCC = SpecialDepsGCC

-- | Construct a 'SpecialDeps' corresponding to the case in which @deps = msvc@
--   is set and @msvc_deps_prefix = …@.
--
--   The @msvc_deps_prefix@ field defines the string which should be stripped
--   from @msvc@'s @/showIncludes@ output. It is only needed if the version of
--   Visual Studio being used is not English. The value of @msvc_deps_prefix@
--   is @"Note: including file: "@ by default.
--
--   @since 0.1.0
{-# INLINE makeSpecialDepsMSVC #-}
makeSpecialDepsMSVC :: Text
                    -> SpecialDeps
makeSpecialDepsMSVC = SpecialDepsMSVC

-- | A prism for the @deps = gcc@ case.
--
--   @since 0.1.0
{-# INLINE _SpecialDepsGCC #-}
_SpecialDepsGCC :: Lens.Prism' SpecialDeps ()
_SpecialDepsGCC = Lens.prism (const makeSpecialDepsGCC)
                  $ \case SpecialDepsGCC -> Right ()
                          owise          -> Left owise

-- | A prism for the @deps = msvc@ / @msvc_deps_prefix = …@ case.
--
--   @since 0.1.0
{-# INLINE _SpecialDepsMSVC #-}
_SpecialDepsMSVC :: Lens.Prism' SpecialDeps Text
_SpecialDepsMSVC = Lens.prism makeSpecialDepsMSVC
                   $ \case (SpecialDepsMSVC prefix) -> Right prefix
                           owise                    -> Left owise

-- | Converts to @{deps: "gcc"}@ or @{deps: "msvc", prefix: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON SpecialDeps where
  toJSON = go
    where
      go SpecialDepsGCC      = Aeson.object ["deps" .= gcc]
      go (SpecialDepsMSVC p) = Aeson.object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Aeson.Value
      (gcc, msvc) = ("gcc", "msvc")

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON SpecialDeps where
  parseJSON = Aeson.withObject "SpecialDeps" $ \o -> do
    deps <- o .: "deps"
    case Text.pack deps of
      "gcc"  -> pure SpecialDepsGCC
      "msvc" -> SpecialDepsMSVC <$> (o .: "prefix")
      owise  -> [ "Invalid deps type ", "\"", owise, "\"; "
                , "should be one of [\"gcc\", \"msvc\"]."
                ] |> mconcat |> Text.unpack |> fail

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable SpecialDeps

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData SpecialDeps

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m SpecialDeps

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m SpecialDeps

--------------------------------------------------------------------------------

-- | A response file to use during rule execution, as documented
--   <https://ninja-build.org/manual.html#ref_rule here>.
--
--   @since 0.1.0
data ResponseFile
  = MkResponseFile
    { _responseFilePath    :: !Path
    , _responseFileContent :: !Text
    }
  deriving (Eq, Ord, Show, Generic)

-- | Construct a 'ResponseFile' with the given 'Path' and content 'Text'.
--
--   @since 0.1.0
{-# INLINE makeResponseFile #-}
makeResponseFile :: Path
                 -- ^ Corresponds to @rspfile@.
                 -> Text
                 -- ^ Corresponds to @rspfile_content@.
                 -> ResponseFile
makeResponseFile = MkResponseFile

-- | A lens for the @rspfile@ field.
--
--   @since 0.1.0
{-# INLINE responseFilePath #-}
responseFilePath :: Lens.Lens' ResponseFile Path
responseFilePath = Lens.lens _responseFilePath
                   $ \(MkResponseFile {..}) x ->
                       MkResponseFile { _responseFilePath = x, .. }

-- | A lens for the @rspfile_content@ field.
--
--   @since 0.1.0
{-# INLINE responseFileContent #-}
responseFileContent :: Lens.Lens' ResponseFile Text
responseFileContent = Lens.lens _responseFileContent
                      $ \(MkResponseFile {..}) x ->
                          MkResponseFile { _responseFileContent = x, .. }

-- | Converts to @{path: …, content: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON ResponseFile where
  toJSON (MkResponseFile {..})
    = [ "path"    .= _responseFilePath
      , "content" .= _responseFileContent
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON ResponseFile where
  parseJSON = Aeson.withObject "ResponseFile"
              $ \o -> MkResponseFile <$> (o .: "path") <*> (o .: "content")

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable ResponseFile

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData ResponseFile

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m ResponseFile

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m ResponseFile

--------------------------------------------------------------------------------
