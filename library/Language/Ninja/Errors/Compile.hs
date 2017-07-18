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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
--   Module      : Language.Ninja.Errors.Compile
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Errors thrown during Ninja compilation.
--
--   @since 0.1.0
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
  , throwUnexpectedMSVCPrefix

    -- * @CompilePoolError@
  , CompilePoolError (..)
  , throwCompilePoolError, throwGenericCompilePoolError
  , throwInvalidPoolDepth
  , throwEmptyPoolName
  ) where

import           Control.Exception         (Exception)
import           Control.Monad.Error.Class (MonadError (throwError))
import           GHC.Generics              (Generic)

import           Data.Text                 (Text)

import qualified Data.Versions             as Ver

import           Flow                      ((.>))

--------------------------------------------------------------------------------

-- | The type of errors encountered during compilation.
--
--   @since 0.1.0
data CompileError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompileError !Text
  | -- | Errors encountered while compiling a 'Meta'.
    --
    --   @since 0.1.0
    CompileMetaError    !CompileMetaError
  | -- | Errors encountered while compiling a 'Build'.
    --
    --   @since 0.1.0
    CompileBuildError   !CompileBuildError
  | -- | Errors encountered while compiling a 'Rule'.
    --
    --   @since 0.1.0
    CompileRuleError    !CompileRuleError
  | -- | Errors encountered while compiling the phony 'HashMap'.
    --
    --   @since 0.1.0
    CompilePhonyError   !CompilePhonyError
  | -- | Errors encountered while compiling the default target 'HashSet'.
    --
    --   @since 0.1.0
    CompileDefaultError !CompileDefaultError
  | -- | Errors encountered while compiling a 'Pool'.
    --
    --   @since 0.1.0
    CompilePoolError    !CompilePoolError
  deriving (Eq, Show, Generic)

-- | Default instance.
--
--   @since 0.1.0
instance Exception CompileError

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompileError'.
--
--   @since 0.1.0
throwCompileError :: (MonadError CompileError m) => CompileError -> m a
throwCompileError = throwError

-- | Throw a generic catch-all 'CompileError'.
--
--   @since 0.1.0
throwGenericCompileError :: (MonadError CompileError m) => Text -> m a
throwGenericCompileError msg = throwCompileError (GenericCompileError msg)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling Ninja metadata.
--
--   @since 0.1.0
data CompileMetaError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompileMetaError !Text
  | -- | @Failed to parse `ninja_required_version`: …@
    --
    --   @since 0.1.0
    VersionParseFailure     !Ver.ParsingError
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompileMetaError'.
--
--   @since 0.1.0
throwCompileMetaError :: (MonadError CompileError m) => CompileMetaError -> m a
throwCompileMetaError = CompileMetaError .> throwCompileError

-- | Throw a generic catch-all 'CompileMetaError'.
--
--   @since 0.1.0
throwGenericCompileMetaError :: (MonadError CompileError m) => Text -> m a
throwGenericCompileMetaError = GenericCompileMetaError .> throwCompileMetaError

-- | Throw a 'VersionParseFailure' error.
--
--   @since 0.1.0
throwVersionParseFailure :: (MonadError CompileError m)
                         => Ver.ParsingError -> m a
throwVersionParseFailure pe = throwCompileMetaError (VersionParseFailure pe)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja phony @build@.
--
--   @since 0.1.0
data CompilePhonyError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompilePhonyError !Text
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompilePhonyError'.
--
--   @since 0.1.0
throwCompilePhonyError :: (MonadError CompileError m)
                       => CompilePhonyError -> m a
throwCompilePhonyError = CompilePhonyError .> throwCompileError

-- | Throw a generic catch-all 'CompilePhonyError'.
--
--   @since 0.1.0
throwGenericCompilePhonyError :: (MonadError CompileError m) => Text -> m a
throwGenericCompilePhonyError = GenericCompilePhonyError
                                .> throwCompilePhonyError

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @default@ statement.
--
--   @since 0.1.0
data CompileDefaultError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompileDefaultError !Text
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompileDefaultError'.
--
--   @since 0.1.0
throwCompileDefaultError :: (MonadError CompileError m)
                         => CompileDefaultError -> m a
throwCompileDefaultError = CompileDefaultError .> throwCompileError

-- | Throw a generic catch-all 'CompileDefaultError'.
--
--   @since 0.1.0
throwGenericCompileDefaultError :: (MonadError CompileError m) => Text -> m a
throwGenericCompileDefaultError = GenericCompileDefaultError
                                  .> throwCompileDefaultError

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @build@ statement.
--
--   @since 0.1.0
data CompileBuildError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompileBuildError !Text
  | -- | @Rule not found: <text>@
    --
    --   @since 0.1.0
    BuildRuleNotFound        !Text
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompileBuildError'.
--
--   @since 0.1.0
throwCompileBuildError :: (MonadError CompileError m)
                       => CompileBuildError -> m a
throwCompileBuildError = CompileBuildError .> throwCompileError

-- | Throw a generic catch-all 'CompileBuildError'.
--
--   @since 0.1.0
throwGenericCompileBuildError :: (MonadError CompileError m) => Text -> m a
throwGenericCompileBuildError = GenericCompileBuildError
                                .> throwCompileBuildError

-- | Throw a 'BuildRuleNotFound' error.
--
--   @since 0.1.0
throwBuildRuleNotFound :: (MonadError CompileError m) => Text -> m a
throwBuildRuleNotFound name = throwCompileBuildError (BuildRuleNotFound name)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @rule@ statement.
--
--   @since 0.1.0
data CompileRuleError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompileRuleError !Text
  | -- | @Lookup failed on rule variable: <text>@
    --
    --   @since 0.1.0
    RuleLookupFailure       !Text
  | -- | @Unknown `deps` value: <text>@
    --
    --   @since 0.1.0
    UnknownDepsValue        !Text
  | -- | @Unexpected `msvc_deps_prefix` for `deps = "<text>"`@
    --
    --   @since 0.1.0
    UnexpectedMSVCPrefix    !Text
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompileRuleError'.
--
--   @since 0.1.0
throwCompileRuleError :: (MonadError CompileError m) => CompileRuleError -> m a
throwCompileRuleError = CompileRuleError .> throwCompileError

-- | Throw a generic catch-all 'CompileRuleError'.
--
--   @since 0.1.0
throwGenericCompileRuleError :: (MonadError CompileError m) => Text -> m a
throwGenericCompileRuleError = GenericCompileRuleError .> throwCompileRuleError

-- | Throw a 'RuleLookupFailure' error.
--
--   @since 0.1.0
throwRuleLookupFailure :: (MonadError CompileError m) => Text -> m a
throwRuleLookupFailure v = throwCompileRuleError (RuleLookupFailure v)

-- | Throw an 'UnknownDeps' error.
--
--   @since 0.1.0
throwUnknownDeps :: (MonadError CompileError m) => Text -> m a
throwUnknownDeps deps = throwCompileRuleError (UnknownDepsValue deps)

-- | Throw an 'UnexpectedMSVCPrefix' error.
--
--   @since 0.1.0
throwUnexpectedMSVCPrefix :: (MonadError CompileError m) => Text -> m a
throwUnexpectedMSVCPrefix deps = throwCompileRuleError
                                 (UnexpectedMSVCPrefix deps)

--------------------------------------------------------------------------------

-- | The type of errors encountered while compiling a Ninja @pool@ statement.
--
--   @since 0.1.0
data CompilePoolError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericCompilePoolError !Text
  | -- | @Invalid pool depth for console: <int>@
    --
    --   @since 0.1.0
    InvalidPoolDepth        !Int
  | -- | @Pool name is an empty string@
    --
    --   @since 0.1.0
    EmptyPoolName
  deriving (Eq, Show, Generic)

-- TODO: add FromJSON/ToJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

-- | Throw a 'CompilePoolError'.
--
--   @since 0.1.0
throwCompilePoolError :: (MonadError CompileError m) => CompilePoolError -> m a
throwCompilePoolError = CompilePoolError .> throwCompileError

-- | Throw a generic catch-all 'CompilePoolError'.
--
--   @since 0.1.0
throwGenericCompilePoolError :: (MonadError CompileError m) => Text -> m a
throwGenericCompilePoolError = GenericCompilePoolError .> throwCompilePoolError

-- | Throw an 'InvalidPoolDepth' error.
--
--   @since 0.1.0
throwInvalidPoolDepth :: (MonadError CompileError m) => Int -> m a
throwInvalidPoolDepth d = throwCompilePoolError (InvalidPoolDepth d)

-- | Throw an 'EmptyPoolName' error.
--
--   @since 0.1.0
throwEmptyPoolName :: (MonadError CompileError m) => m a
throwEmptyPoolName = throwCompilePoolError EmptyPoolName

--------------------------------------------------------------------------------
