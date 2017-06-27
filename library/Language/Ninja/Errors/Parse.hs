-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Errors/Parse.hs
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
--   Module      : Language.Ninja.Errors.Parse
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Errors thrown during Ninja compilation.
module Language.Ninja.Errors.Parse
  ( -- * @ParseError@
    ParseError (..)
  , throwParseError, throwGenericParseError
  ) where

import           Control.Exception         (Exception)
import           Control.Monad.Error.Class (MonadError (..))
import           GHC.Generics              (Generic)

import           Data.Text                 (Text)

import           Flow                      ((.>), (|>))

--------------------------------------------------------------------------------

-- | The type of errors encountered during parsing.
data ParseError
  = -- | Generic catch-all error constructor. Avoid using this.
    GenericParseError !Text
  deriving (Eq, Show, Generic)

-- | Default instance.
instance Exception ParseError

-- | Throw a 'ParseError'.
throwParseError :: (MonadError ParseError m) => ParseError -> m a
throwParseError = throwError

-- | Throw a generic catch-all 'ParseError'.
throwGenericParseError :: (MonadError ParseError m) => Text -> m a
throwGenericParseError msg = throwParseError (GenericParseError msg)

--------------------------------------------------------------------------------
