-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Path.hs
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

-- |
--   Module      : Language.Ninja.Misc.Path
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   A datatype for Unix path strings.
module Language.Ninja.Misc.Path
  ( Path, makePath, pathIText, pathText
  ) where

import           Language.Ninja.Misc.IText

import           Data.Text                 (Text)

import           Data.Aeson                as Aeson

import           Data.Hashable             (Hashable (..))
import           Data.String               (IsString (..))
import           GHC.Generics              (Generic)

import           Control.Lens.Getter
import           Control.Lens.Iso

import           Flow

--------------------------------------------------------------------------------

-- | This type represents a Unix path string.
newtype Path
  = MkPath
    { _pathIText :: IText
    }
  deriving ( Eq, Ord, Show, Read, IsString, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

-- | Construct a 'Path' from some 'Text'.
makePath :: Text -> Path
makePath = view itext .> MkPath

-- | An isomorphism between a 'Path' and its underlying 'IText'.
pathIText :: Iso' Path IText
pathIText = iso _pathIText MkPath

-- | An isomorphism that gives access to a 'Text'-typed view of a 'Path',
--   even though the underlying data has type 'IText'.
--
--   This is equivalent to @pathIText . from itext@.
pathText :: Iso' Path Text
pathText = pathIText . from itext

--------------------------------------------------------------------------------
