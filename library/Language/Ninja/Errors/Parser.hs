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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

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
import           Control.Monad.Error.Class (MonadError (throwError))
import           GHC.Generics              (Generic)

import           Data.Text                 (Text)

import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Aeson

import qualified Text.Megaparsec           as M

import           Data.Foldable             (toList)

import           Flow                      ((|>))

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

-- FIXME: remove unused errors here

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

-- | Default instance.
--
--   @since 0.1.0
instance Exception ParseError

-- | Converts to @{tag: …, value: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON ParseError where
  toJSON = go
    where
      go (GenericParseError t)      = obj "generic-parse-error"      t
      go (LexBindingFailure t)      = obj "lex-binding-failure"      t
      go LexExpectedColon           = obj "lex-expected-colon"       nullJ
      go LexUnexpectedDollar        = obj "lex-unexpected-dollar"    nullJ
      go (LexUnexpectedSeparator c) = obj "lex-unexpected-separator" c
      go (LexParsecError pe)        = obj "lex-parsec-error"         (peJ pe)
      go (ParseBadDepthField t)     = obj "parse-bad-depth-field"    t
      go (ParseUnexpectedBinding t) = obj "parse-unexpected-binding" t

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

      nullJ = Aeson.Null :: Aeson.Value

-- FIXME: add a FromJSON instance
-- FIXME: add Arbitrary instance
-- FIXME: add (Co)Serial instance

--------------------------------------------------------------------------------
