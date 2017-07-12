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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Misc.Command
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype representing a POSIX @sh@ command line.
--
--   @since 0.1.0
module Language.Ninja.Misc.Command
  ( Command, makeCommand, commandText
  ) where

import           Data.Text              (Text)

import qualified Data.Aeson             as Aeson

import           Control.DeepSeq        (NFData)
import           Data.Hashable          (Hashable)
import           GHC.Generics           (Generic)

import           Test.SmallCheck.Series ((>>-))
import qualified Test.SmallCheck.Series as SC

import qualified Control.Lens           as Lens

import           Flow                   ((.>))

--------------------------------------------------------------------------------

-- | This type represents a POSIX @sh@ command line.
--
--   @since 0.1.0
newtype Command
  = MkCommand
    { _commandText :: Text
    }
  deriving ( Eq, Ord, Show, Read, Generic, Hashable, NFData
           , Aeson.ToJSON, Aeson.FromJSON )

-- | Constructor for a 'Command'.
--
--   @since 0.1.0
{-# INLINE makeCommand #-}
makeCommand :: Text -> Command
makeCommand = MkCommand

-- | An isomorphism between a 'Command' and its underlying 'Text'.
--
--   @since 0.1.0
{-# INLINE commandText #-}
commandText :: Lens.Iso' Command Text
commandText = Lens.iso _commandText MkCommand

-- | Uses the underlying 'Text' instance.
--
--   @since 0.1.0
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m Command where
  series = SC.newtypeCons MkCommand

-- | Uses the underlying 'Text' instance.
--
--   @since 0.1.0
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m Command where
  coseries rs = SC.newtypeAlts rs
                >>- \f -> pure (_commandText .> f)

--------------------------------------------------------------------------------
