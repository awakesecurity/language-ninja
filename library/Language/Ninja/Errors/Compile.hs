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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

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

import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Aeson

import qualified Text.Megaparsec           as M

import qualified Data.Versions             as Ver

import           Data.Foldable             (toList)

import           Flow                      ((.>), (|>))

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

-- | Default instance.
--
--   @since 0.1.0
instance Exception CompileError

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompileError where
  toJSON = go
    where
      go (GenericCompileError text) = obj "generic-compile-error" text
      go (CompileMetaError    cme)  = obj "compile-meta-error"    cme
      go (CompileBuildError   cbe)  = obj "compile-build-error"   cbe
      go (CompileRuleError    cre)  = obj "compile-rule-error"    cre
      go (CompilePhonyError   cpe)  = obj "compile-phony-error"   cpe
      go (CompileDefaultError cde)  = obj "compile-default-error" cde
      go (CompilePoolError    cpe)  = obj "compile-pool-error"    cpe

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompileMetaError where
  toJSON = go
    where
      go (GenericCompileMetaError t) = obj "generic-compile-meta-error" t
      go (VersionParseFailure     e) = obj "version-parse-failure"      (peJ e)

      -- TODO: deduplicate against the implementation in Errors.Parser

      peJ :: M.ParseError Char M.Dec -> Aeson.Value
      peJ (decomposePE -> (pos, custom, unexpected, expected))
        = [ "pos"        .= (posJ     <$> pos)
          , "unexpected" .= (errItemJ <$> unexpected)
          , "expected"   .= (errItemJ <$> expected)
          , "custom"     .= (decJ     <$> custom)
          ] |> Aeson.object

      decomposePE :: M.ParseError Char M.Dec
                  -> ( [M.SourcePos], [M.Dec]
                     , [M.ErrorItem Char], [M.ErrorItem Char] )
      decomposePE (M.ParseError {..})
        = ( toList errorPos, toList errorCustom
          , toList errorUnexpected, toList errorExpected )

      posJ :: M.SourcePos -> Aeson.Value
      posJ (M.SourcePos {..}) = [ "name"   .= sourceName
                                , "line"   .= M.unPos sourceLine
                                , "column" .= M.unPos sourceColumn
                                ] |> Aeson.object

      errItemJ :: M.ErrorItem Char -> Aeson.Value
      errItemJ (M.Tokens xs) = Aeson.toJSON (toList xs)
      errItemJ (M.Label  xs) = Aeson.toJSON (toList xs)
      errItemJ M.EndOfInput  = "eof"

      decJ :: M.Dec -> Aeson.Value
      decJ (M.DecFail message)        = [ "message"  .= message
                                        ] |> Aeson.object |> obj "fail"
      decJ (M.DecIndentation ord x y) = [ "ordering" .= ord
                                        , "start"    .= M.unPos x
                                        , "end"      .= M.unPos y
                                        ] |> Aeson.object |> obj "indentation"

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompilePhonyError where
  toJSON = go
    where
      go (GenericCompilePhonyError t) = obj "generic-compile-phony-error" t

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompileDefaultError where
  toJSON = go
    where
      go (GenericCompileDefaultError t) = obj "generic-compile-default-error" t

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompileBuildError where
  toJSON = go
    where
      go (GenericCompileBuildError t) = obj "generic-compile-build-error" t
      go (BuildRuleNotFound        n) = obj "build-rule-not-found"        n

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompileRuleError where
  toJSON = go
    where
      go (GenericCompileRuleError t) = obj "generic-compile-build-error" t
      go (RuleLookupFailure       n) = obj "rule-lookup-failure"         n
      go (UnknownDepsValue        d) = obj "unknown-deps-value"          d
      go (UnexpectedMSVCPrefix    d) = obj "unexpected-msvc-prefix"      d

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

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

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON CompilePoolError where
  toJSON = go
    where
      go (GenericCompilePoolError t) = obj "generic-compile-pool-error" t
      go (InvalidPoolDepth        d) = obj "invalid-pool-depth"         d
      go EmptyPoolName               = obj "empty-pool-name"            nullJ

      obj :: (Aeson.ToJSON x) => Text -> x -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

      nullJ = Aeson.Null :: Aeson.Value

-- TODO: add FromJSON instance
-- TODO: add Arbitrary instance
-- TODO: add (Co)Serial instance

--------------------------------------------------------------------------------
