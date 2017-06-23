-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Command.hs
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--   Module      : Language.Ninja.Misc.Command
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype representing a POSIX @sh@ command line.
module Language.Ninja.Misc.Command
  ( Command, makeCommand, commandText
  ) where

import           Data.Text        (Text)

import           Data.Aeson       (FromJSON, ToJSON)

import           Data.Hashable    (Hashable)
import           GHC.Generics     (Generic)

import           Control.Lens.Iso (Iso')
import qualified Control.Lens

--------------------------------------------------------------------------------

-- | This type represents a POSIX @sh@ command line.
newtype Command
  = MkCommand
    { _commandText :: Text
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable
           , ToJSON, FromJSON )

-- | Constructor for a 'Command'.
makeCommand :: Text -> Command
makeCommand = MkCommand

-- | An isomorphism between a 'Command' and its underlying 'Text'.
commandText :: Iso' Command Text
commandText = Control.Lens.iso _commandText MkCommand

--------------------------------------------------------------------------------
