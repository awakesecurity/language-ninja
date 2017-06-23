-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Errors/Compile.hs
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

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--   Module      : Language.Ninja.Errors.Compile
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Errors.Compile
  ( -- * @CompileError@
    CompileError (..)
  , throwCompileError, throwGenericCompileError

    -- * @CompileMetaError@
  , CompileMetaError (..)
  , throwCompileMetaError, throwGenericCompileMetaError
  , throwVersionParseFailure

    -- * @CompilePhonyError@
  , CompilePhonyError (..)
  , throwCompilePhonyError, throwGenericCompilePhonyError

    -- * @CompileDefaultError@
  , CompileDefaultError (..)
  , throwCompileDefaultError, throwGenericCompileDefaultError

    -- * @CompileBuildError@
  , CompileBuildError (..)
  , throwCompileBuildError, throwGenericCompileBuildError
  , throwBuildRuleNotFound

    -- * @CompileRuleError@
  , CompileRuleError (..)
  , throwCompileRuleError, throwGenericCompileRuleError
  , throwRuleLookupFailure
  , throwUnknownDeps

    -- * @CompilePoolError@
  , CompilePoolError (..)
  , throwCompilePoolError, throwGenericCompilePoolError
  , throwInvalidPoolDepth
  , throwEmptyPoolName
  ) where

import           Control.Applicative          ((<|>))
import           Control.Arrow                (first)

import           Control.Lens.Getter          ((^.))
import           Control.Lens.Setter          ((.~))

import           Control.Exception            (Exception)
import           Control.Monad.Catch          (MonadThrow (..))

import           Data.Char                    (isSpace)
import           Data.Functor                 (void)
import           Data.Maybe                   (fromMaybe, isJust)
import           Data.Monoid                  (Endo (..), (<>))

import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.Aeson.Types             as Aeson

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC8

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC8

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T

import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM

import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS

import           Flow                         ((.>), (|>))

import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)

import qualified Data.Versions                as Ver

import           Language.Ninja.AST
                 (Build, Command, Dependency, DependencyType (..), Meta, Ninja,
                 Output, OutputType (..), Pool, ResponseFile, Rule,
                 SpecialDeps, Target)
import qualified Language.Ninja.AST           as Ninja
import           Language.Ninja.Env           (askEnv)
import qualified Language.Ninja.Misc.Positive as Ninja
import qualified Language.Ninja.Parse         as Ninja
import           Language.Ninja.Types
                 (Env, FileText, PBuild, PNinja, PRule)
import qualified Language.Ninja.Types         as Ninja

--------------------------------------------------------------------------------

-- | The type of errors encountered during compilation.
data CompileError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompileError !Text
  | -- | Errors encountered while compiling a 'Meta'.
    CompileMetaError    !CompileMetaError
  | -- | Errors encountered while compiling a 'Build'.
    CompileBuildError   !CompileBuildError
  | -- | Errors encountered while compiling a 'Rule'.
    CompileRuleError    !CompileRuleError
  | -- | Errors encountered while compiling the phony 'HashMap'.
    CompilePhonyError   !CompilePhonyError
  | -- | Errors encountered while compiling the default target 'HashSet'.
    CompileDefaultError !CompileDefaultError
  | -- | Errors encountered while compiling a 'Pool'.
    CompilePoolError    !CompilePoolError
  deriving (Eq, Show, Generic)

-- | Default instance.
instance Exception CompileError

-- | Throw a 'CompileError'.
throwCompileError :: (MonadThrow m) => CompileError -> m a
throwCompileError = throwM

-- | Throw a generic catch-all 'CompileError'.
throwGenericCompileError :: (MonadThrow m) => Text -> m a
throwGenericCompileError msg = throwM (GenericCompileError msg)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling Ninja metadata.
data CompileMetaError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompileMetaError !Text
  | -- | @Failed to parse `ninja_required_version`: …@
    VersionParseFailure     !Ver.ParsingError
  deriving (Eq, Show, Generic)

-- | Throw a 'CompileMetaError'.
throwCompileMetaError :: (MonadThrow m) => CompileMetaError -> m a
throwCompileMetaError = CompileMetaError .> throwM

-- | Throw a generic catch-all 'CompileMetaError'.
throwGenericCompileMetaError :: (MonadThrow m) => Text -> m a
throwGenericCompileMetaError = GenericCompileMetaError .> throwCompileMetaError

-- | Throw a 'VersionParseFailure' error.
throwVersionParseFailure :: (MonadThrow m) => Ver.ParsingError -> m a
throwVersionParseFailure pe = throwCompileMetaError (VersionParseFailure pe)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja phony @build@.
data CompilePhonyError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompilePhonyError !Text
  deriving (Eq, Show, Generic)

-- | Throw a 'CompilePhonyError'.
throwCompilePhonyError :: (MonadThrow m) => CompilePhonyError -> m a
throwCompilePhonyError = CompilePhonyError .> throwM

-- | Throw a generic catch-all 'CompilePhonyError'.
throwGenericCompilePhonyError :: (MonadThrow m) => Text -> m a
throwGenericCompilePhonyError = GenericCompilePhonyError
                                .> throwCompilePhonyError

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @default@ statement.
data CompileDefaultError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompileDefaultError !Text
  deriving (Eq, Show, Generic)

-- | Throw a 'CompileDefaultError'.
throwCompileDefaultError :: (MonadThrow m) => CompileDefaultError -> m a
throwCompileDefaultError = CompileDefaultError .> throwM

-- | Throw a generic catch-all 'CompileDefaultError'.
throwGenericCompileDefaultError :: (MonadThrow m) => Text -> m a
throwGenericCompileDefaultError = GenericCompileDefaultError
                                  .> throwCompileDefaultError

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @build@ statement.
data CompileBuildError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompileBuildError !Text
  | -- | @Rule not found: <text>@
    BuildRuleNotFound        !Text
  deriving (Eq, Show, Generic)

-- | Throw a 'CompileBuildError'.
throwCompileBuildError :: (MonadThrow m) => CompileBuildError -> m a
throwCompileBuildError = CompileBuildError .> throwM

-- | Throw a generic catch-all 'CompileBuildError'.
throwGenericCompileBuildError :: (MonadThrow m) => Text -> m a
throwGenericCompileBuildError = GenericCompileBuildError
                                .> throwCompileBuildError

-- | Throw a 'BuildRuleNotFound' error.
throwBuildRuleNotFound :: (MonadThrow m) => Text -> m a
throwBuildRuleNotFound name = throwCompileBuildError (BuildRuleNotFound name)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @rule@ statement.
data CompileRuleError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompileRuleError !Text
  | -- | @Lookup failed on rule variable: <text>@
    RuleLookupFailure       !Text
  | -- | @Unknown `deps` value: <text>@
    UnknownDepsValue        !Text
  | -- | @Unexpected `msvc_deps_prefix` for `deps = "<text>"`@
    UnexpectedMSVCPrefix    !Text
  deriving (Eq, Show, Generic)

-- | Throw a 'CompileRuleError'.
throwCompileRuleError :: (MonadThrow m) => CompileRuleError -> m a
throwCompileRuleError = CompileRuleError .> throwM

-- | Throw a generic catch-all 'CompileRuleError'.
throwGenericCompileRuleError :: (MonadThrow m) => Text -> m a
throwGenericCompileRuleError = GenericCompileRuleError .> throwCompileRuleError

-- | Throw a 'RuleLookupFailure' error.
throwRuleLookupFailure :: (MonadThrow m) => Text -> m a
throwRuleLookupFailure v = throwCompileRuleError (RuleLookupFailure v)

-- | Throw an 'UnknownDeps' error.
throwUnknownDeps :: (MonadThrow m) => Text -> m a
throwUnknownDeps deps = throwCompileRuleError (UnknownDepsValue deps)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @pool@ statement.
data CompilePoolError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericCompilePoolError !Text
  | -- | @Invalid pool depth for console: <int>@
    InvalidPoolDepth     !Int
  | -- | @Pool name is an empty string@
    EmptyPoolName
  deriving (Eq, Show, Generic)

-- | Throw a 'CompilePoolError'.
throwCompilePoolError :: (MonadThrow m) => CompilePoolError -> m a
throwCompilePoolError = CompilePoolError .> throwM

-- | Throw a generic catch-all 'CompilePoolError'.
throwGenericCompilePoolError :: (MonadThrow m) => Text -> m a
throwGenericCompilePoolError = GenericCompilePoolError .> throwCompilePoolError

-- | Throw an 'InvalidPoolDepth' error.
throwInvalidPoolDepth :: (MonadThrow m) => Int -> m a
throwInvalidPoolDepth d = throwCompilePoolError (InvalidPoolDepth d)

-- | Throw an 'EmptyPoolName' error.
throwEmptyPoolName :: (MonadThrow m) => m a
throwEmptyPoolName = throwCompilePoolError EmptyPoolName

--------------------------------------------------------------------------------
