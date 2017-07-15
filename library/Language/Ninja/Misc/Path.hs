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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Misc.Path
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Unix path strings.
--
--   @since 0.1.0
module Language.Ninja.Misc.Path
  ( Path, makePath, pathIText, pathText, pathString, pathFP
  ) where

import           Language.Ninja.Misc.IText (IText)
import qualified Language.Ninja.Misc.IText as Ninja

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import qualified Filesystem.Path.CurrentOS as FP

import qualified Data.Aeson                as Aeson

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           Data.String               (IsString)
import           GHC.Generics              (Generic)
import           Test.SmallCheck.Series    ((>>-))
import qualified Test.SmallCheck.Series    as SC

import qualified Control.Lens              as Lens

import           Flow                      ((.>))

--------------------------------------------------------------------------------

-- | This type represents a Unix path string.
--
--   @since 0.1.0
newtype Path
  = MkPath
    { _pathIText :: IText
    }
  deriving ( Eq, Ord, Show, Read, IsString, Generic, Hashable, NFData
           , Aeson.ToJSON, Aeson.FromJSON, Aeson.ToJSONKey, Aeson.FromJSONKey )

-- | Construct a 'Path' from some 'Text'.
--
--   @since 0.1.0
{-# INLINE makePath #-}
makePath :: Text -> Path
makePath = Lens.view Ninja.itext .> MkPath

-- | An isomorphism between a 'Path' and its underlying 'IText'.
--
--   @since 0.1.0
{-# INLINE pathIText #-}
pathIText :: Lens.Iso' Path IText
pathIText = Lens.iso _pathIText MkPath

-- | An isomorphism that gives access to a 'Text'-typed view of a 'Path',
--   even though the underlying data has type 'IText'.
--
--   This is equivalent to @pathIText . from Ninja.itext@.
--
--   @since 0.1.0
{-# INLINE pathText #-}
pathText :: Lens.Iso' Path Text
pathText = pathIText . Lens.from Ninja.itext

-- | An isomorphism that gives access to a 'String'-typed view of a 'Path'.
--
--   @since 0.1.0
{-# INLINE pathString #-}
pathString :: Lens.Iso' Path String
pathString = pathText . Lens.iso Text.unpack Text.pack

-- | An isomorphism between a 'Path' and a 'FP.FilePath' from @system-filepath@.
--   This uses 'FP.decodeString' and 'FP.encodeString', so all the caveats on
--   those functions apply here.
--
--   @since 0.1.0
{-# INLINE pathFP #-}
pathFP :: Lens.Iso' Path FP.FilePath
pathFP = pathString . Lens.iso FP.decodeString FP.encodeString

-- | Uses the underlying 'IText' instance.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Path where
  series = SC.newtypeCons MkPath

-- | Uses the underlying 'IText' instance.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Path where
  coseries rs = SC.newtypeAlts rs
                >>- \f -> pure (_pathIText .> f)

--------------------------------------------------------------------------------
