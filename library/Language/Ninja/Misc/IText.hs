-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/IText.hs
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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Misc.IText
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   An interned text type.
--
--   @since 0.1.0
module Language.Ninja.Misc.IText
  ( IText, uninternText, internText, itext
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

import           Data.Aeson
                 (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Types       as Aeson

import qualified Data.Interned          as Interned
import qualified Data.Interned.Text     as Interned (InternedText)

import           Control.DeepSeq        (NFData (..))
import           Data.Hashable          (Hashable (..))
import           Data.String            (IsString (..))
import           GHC.Generics           (Generic)
import qualified Test.SmallCheck.Series as SC

import qualified Control.Lens
import           Control.Lens.Iso       (Iso')

import           Control.Arrow          (first)
import           Flow                   ((.>), (|>))

--------------------------------------------------------------------------------

-- | An interned (hash-consed) text type.
--   This is a newtype over 'Interned.InternedText' from the @intern@ package.
--
--   @since 0.1.0
newtype IText
  = MkIText Interned.InternedText
  deriving (Eq, IsString, Generic)

-- | Get the 'Text' corresponding to the given 'IText' value.
--
--   prop> internText (uninternText x) = x
--
--   >>> uninternText ("foobar" :: IText)
--   "foobar"
--
--   @since 0.1.0
{-# INLINE uninternText #-}
uninternText :: IText -> Text
uninternText (MkIText i) = Interned.unintern i

-- | Intern a 'Text' value, resulting in an 'IText' value.
--
--   prop> uninternText (internText x) = x
--
--   >>> internText ("foobar" :: Text)
--   "foobar"
--
--   @since 0.1.0
{-# INLINE internText #-}
internText :: Text -> IText
internText = Interned.intern .> MkIText

-- | An 'Iso' between 'Text' and 'IText'.
--
--   prop> ((fromString x) ^. itext) = T.pack x
--
--   >>> (("foobar" :: Text) ^. itext) :: IText
--   "foobar"
--
--   >>> (("foobar" :: IText) ^. from itext) :: Text
--   "foobar"
--
--   @since 0.1.0
{-# INLINE itext #-}
itext :: Iso' Text IText
itext = Control.Lens.iso internText uninternText

-- | The 'Ord' instance in @intern@ compares hashes rather than values.
--
--   @since 0.1.0
instance Ord IText where
  compare itA itB = compare (uninternText itA) (uninternText itB)

-- | Displays an 'IText' such that 'fromString' is inverse to 'show'.
--
--   @since 0.1.0
instance Show IText where
  show (MkIText i) = show i

-- | Inverse of the 'Show' instance.
--
--   @since 0.1.0
instance Read IText where
  readsPrec i = readsPrec i .> map (first (fromString .> MkIText))

-- | Uses the 'Hashable' instance for 'Text'. Not very efficient.
--
--   FIXME: perhaps switch to hashing the identifier, since this is likely
--   pretty hot code given all the @HashMap Target …@ types all over the place.
--
--   @since 0.1.0
instance Hashable IText where
  hashWithSalt n = uninternText .> hashWithSalt n

-- | Defined by @rnf a = seq a ()@, since 'IText' is a newtype of strict types.
--
--   @since 0.1.0
instance NFData IText where
  rnf a = seq a ()

-- | Converts to JSON string via 'uninternText'.
--
--   @since 0.1.0
instance ToJSON IText where
  toJSON = uninternText .> toJSON

-- | Inverse of the 'ToJSON' instance.
--
--   @since 0.1.0
instance FromJSON IText where
  parseJSON = Aeson.withText "IText" (internText .> pure)

-- | Converts to JSON string via 'uninternText'.
--
--   @since 0.1.0
instance ToJSONKey IText where
  toJSONKey = Aeson.toJSONKeyText uninternText

-- | Inverse of the 'ToJSONKey' instance.
--
--   @since 0.1.0
instance FromJSONKey IText where
  fromJSONKey = Aeson.mapFromJSONKeyFunction internText fromJSONKey

-- | Uses the 'Text' instance.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m IText where
  series = SC.series |> fmap internText

-- | Uses the 'Text' instance.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m IText where
  coseries = SC.coseries .> fmap (\f int -> f (uninternText int))

--------------------------------------------------------------------------------
