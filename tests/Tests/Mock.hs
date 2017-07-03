-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests/Mock.hs
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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}

-- |
--   Module      : Tests.Mock
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Tests.Mock
  ( module Tests.Mock -- FIXME: specific export list
  ) where

import           Data.Monoid                  ((<>))

import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS

import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)

import qualified Language.Ninja.AST           as AST
import           Language.Ninja.Errors.Parser (ParseError)
import           Language.Ninja.Misc.Path     (Path)
import           Language.Ninja.Mock.ReadFile (MonadReadFile (..))

import           Control.Exception            (evaluate)
import qualified Control.Monad.Mock           as Mock
import           Data.Type.Equality           ((:~:) (Refl))

--------------------------------------------------------------------------------

data FSAction r where
  ReadFile  :: !Path          -> FSAction Text
  WriteFile :: !Path -> !Text -> FSAction ()

deriving instance Eq (FSAction r)
deriving instance Show (FSAction r)

instance Mock.Action FSAction where
  eqAction (a@(ReadFile  {})) (b@(ReadFile  {})) | (a == b) = Just Refl
  eqAction (a@(WriteFile {})) (b@(WriteFile {})) | (a == b) = Just Refl
  eqAction _                  _                             = Nothing

instance (Monad m) => MonadReadFile (Mock.MockT FSAction m) where
  readFile path = Mock.mockAction "readFile" (ReadFile path)

--------------------------------------------------------------------------------
