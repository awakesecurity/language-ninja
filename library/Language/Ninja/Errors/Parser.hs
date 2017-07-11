-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Errors/Parser.hs
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
--   Module      : Language.Ninja.Errors.Parser
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Errors thrown during Ninja parsing.
--
--   @since 0.1.0
module Language.Ninja.Errors.Parser
  ( -- * @ParseError@
    ParseError (..)
  , throwParseError, throwGenericParseError
  , throwLexBindingFailure
  , throwLexExpectedColon
  , throwLexUnexpectedDollar
  , throwLexUnexpectedSeparator
  , throwLexParsecError
  , throwParseBadDepthField
  , throwParseUnexpectedBinding
  ) where

import           Control.Exception         (Exception)
import           Control.Monad.Error.Class (MonadError (..))
import           GHC.Generics              (Generic)

import           Data.Text                 (Text)

import           Flow                      ((.>), (|>))

import qualified Text.Megaparsec           as M

import qualified Language.Ninja.AST.Expr   as AST

--------------------------------------------------------------------------------

-- | The type of errors encountered during parsing.
--
--   @since 0.1.0
data ParseError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericParseError      !Text
  | -- | @Lexer failed at binding: <text>@
    --
    --   @since 0.1.0
    LexBindingFailure      !Text
  | -- | @Expected a colon@
    --
    --   @since 0.1.0
    LexExpectedColon
  | -- | @Unexpected $ followed by unexpected stuff@
    --
    --   @since 0.1.0
    LexUnexpectedDollar
  | -- | Lexer expected a separator character but found something else
    --
    --   @since 0.1.0
    LexUnexpectedSeparator Char
  | -- | Any other lexer error.
    --
    --   @since 0.1.0
    LexParsecError         !(M.ParseError Char M.Dec)
  | -- | @Could not parse depth field in pool, got: <text>@
    --
    --   @since 0.1.0
    ParseBadDepthField     !Text
  | -- | @Unexpected binding defining <text>@
    --
    --   @since 0.1.0
    ParseUnexpectedBinding !Text
  deriving (Eq, Show, Generic)

-- | Default instance.
--
--   @since 0.1.0
instance Exception ParseError

-- | Throw a 'ParseError'.
--
--   @since 0.1.0
throwParseError :: (MonadError ParseError m) => ParseError -> m a
throwParseError = throwError

-- | Throw a generic catch-all 'ParseError'.
--
--   @since 0.1.0
throwGenericParseError :: (MonadError ParseError m) => Text -> m a
throwGenericParseError msg = throwParseError (GenericParseError msg)

-- | Throw a 'LexBindingFailure' error.
--
--   @since 0.1.0
throwLexBindingFailure :: (MonadError ParseError m) => Text -> m a
throwLexBindingFailure t = throwParseError (LexBindingFailure t)

-- | Throw a 'LexExpectedColon' error.
--
--   @since 0.1.0
throwLexExpectedColon :: (MonadError ParseError m) => m a
throwLexExpectedColon = throwParseError LexExpectedColon

-- | Throw a 'LexUnexpectedColon' error.
--
--   @since 0.1.0
throwLexUnexpectedDollar :: (MonadError ParseError m) => m a
throwLexUnexpectedDollar = throwParseError LexUnexpectedDollar

-- | Throw a 'LexUnexpectedSeparator' error.
--
--   @since 0.1.0
throwLexUnexpectedSeparator :: (MonadError ParseError m) => Char -> m a
throwLexUnexpectedSeparator c = throwParseError (LexUnexpectedSeparator c)

-- | Throw a 'LexParsecError' error.
--
--   @since 0.1.0
throwLexParsecError :: (MonadError ParseError m)
                    => M.ParseError Char M.Dec -> m a
throwLexParsecError pe = throwParseError (LexParsecError pe)

-- | Throw a 'ParseBadDepthField' error.
--
--   @since 0.1.0
throwParseBadDepthField :: (MonadError ParseError m) => Text -> m a
throwParseBadDepthField t = throwParseError (ParseBadDepthField t)

-- | Throw a 'ParseUnexpectedBinding' error.
--
--   @since 0.1.0
throwParseUnexpectedBinding :: (MonadError ParseError m) => Text -> m a
throwParseUnexpectedBinding t = throwParseError (ParseUnexpectedBinding t)

--------------------------------------------------------------------------------
