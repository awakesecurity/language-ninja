-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Path.hs
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
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Misc.Path
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Unix path strings.
module Language.Ninja.Misc.Path
  ( Path, makePath, pathIText, pathText, pathString, pathFP
  ) where

import           Language.Ninja.Misc.IText (IText)
import qualified Language.Ninja.Misc.IText as Ninja

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import qualified Filesystem.Path.CurrentOS as FP

import           Data.Aeson
                 (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson                as Aeson

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           Data.String               (IsString (..))
import           GHC.Generics              (Generic)
import           Test.SmallCheck.Series    ((>>-))
import qualified Test.SmallCheck.Series    as SC

import           Control.Lens.Getter       (view)
import           Control.Lens.Iso          (Iso', from, iso)

import           Flow                      ((.>))

--------------------------------------------------------------------------------

-- | This type represents a Unix path string.
newtype Path
  = MkPath
    { _pathIText :: IText
    }
  deriving ( Eq, Ord, Show, Read, IsString, Generic, Hashable, NFData
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

-- | Construct a 'Path' from some 'Text'.
{-# INLINE makePath #-}
makePath :: Text -> Path
makePath = view Ninja.itext .> MkPath

-- | An isomorphism between a 'Path' and its underlying 'IText'.
{-# INLINE pathIText #-}
pathIText :: Iso' Path IText
pathIText = iso _pathIText MkPath

-- | An isomorphism that gives access to a 'Text'-typed view of a 'Path',
--   even though the underlying data has type 'IText'.
--
--   This is equivalent to @pathIText . from Ninja.itext@.
{-# INLINE pathText #-}
pathText :: Iso' Path Text
pathText = pathIText . from Ninja.itext

-- | An isomorphism that gives access to a 'String'-typed view of a 'Path'.
{-# INLINE pathString #-}
pathString :: Iso' Path String
pathString = pathText . iso Text.unpack Text.pack

-- | An isomorphism between a 'Path' and a 'FP.FilePath' from @system-filepath@.
--   This uses 'FP.decodeString' and 'FP.encodeString', so all the caveats on
--   those functions apply here.
{-# INLINE pathFP #-}
pathFP :: Iso' Path FP.FilePath
pathFP = pathString . iso FP.decodeString FP.encodeString

-- | Uses the underlying 'IText' instance.
instance ( Monad m
         , SC.Serial m Text
         ) => SC.Serial m Path where
  series = SC.newtypeCons MkPath

-- | Uses the underlying 'IText' instance.
instance ( Monad m
         , SC.CoSerial m Text
         ) => SC.CoSerial m Path where
  coseries rs = SC.newtypeAlts rs
                >>- \f -> pure (_pathIText .> f)

--------------------------------------------------------------------------------
