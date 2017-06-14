-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/IText.hs
--
-- License:
--     Copyright 2017 Awake Networks
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
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
--   Module      : Language.Ninja.Misc.IText
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   An interned text type.
module Language.Ninja.Misc.IText
  ( IText, uninternText, internText
  ) where

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.Aeson         as Aeson
import qualified Data.Aeson.Types   as Aeson

import qualified Data.Interned      as Interned
import qualified Data.Interned.Text as Interned (InternedText)

import           Data.Data          (Data)
import           Data.Hashable      (Hashable (..))
import           Data.String        (IsString (..))
import           GHC.Generics       (Generic)

import           Control.Arrow      (first)
import           Flow

--------------------------------------------------------------------------------

-- | An interned (hash-consed) text type.
--   This is a newtype over 'Interned.InternedText' from the @intern@ package.
newtype IText
  = MkIText Interned.InternedText
  deriving (Eq, Ord, IsString, Generic)

-- | Get the 'Text' corresponding to the given 'Interned' value.
uninternText :: IText -> Text
uninternText (MkIText i) = Interned.unintern i

-- | Intern a 'Text' value, resulting in an 'Interned' value.
internText :: Text -> IText
internText = Interned.intern .> MkIText

-- | Displays an 'Interned' such that 'fromString' is inverse to 'show'.
instance Show IText where
  show (MkIText i) = show i

-- | Inverse of the 'Show' instance.
instance Read IText where
  readsPrec i = readsPrec i .> map (first (fromString .> MkIText))

-- | Uses the 'Hashable' instance for 'Text'. Not very efficient.
instance Hashable IText where
  hashWithSalt n = uninternText .> hashWithSalt n

-- | Converts to JSON string via 'uninternText'.
instance ToJSON IText where
  toJSON = uninternText .> toJSON

-- | Inverse of the 'ToJSON' instance.
instance FromJSON IText where
  parseJSON = withText "Interned" (internText .> pure)

-- | Converts to JSON string via 'uninternText'.
instance ToJSONKey IText where
  toJSONKey = Aeson.toJSONKeyText uninternText

-- | Inverse of the 'ToJSONKey' instance.
instance FromJSONKey IText where
  fromJSONKey = Aeson.mapFromJSONKeyFunction internText fromJSONKey

--------------------------------------------------------------------------------
