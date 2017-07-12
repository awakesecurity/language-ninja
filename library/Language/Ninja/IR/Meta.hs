-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/IR/Meta.hs
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.IR.Meta
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A datatype for Ninja top-level variables and other metadata.
--
--   @since 0.1.0
module Language.Ninja.IR.Meta
  ( -- * @Meta@
    Meta, makeMeta, metaReqVersion, metaBuildDir
  ) where

import           Data.Aeson               ((.:), (.=))
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Types         as Aeson

import           Data.Text                (Text)

import           Control.DeepSeq          (NFData)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)
import qualified Test.SmallCheck.Series   as SC

import qualified Data.Versions            as Ver

import qualified Text.Megaparsec          as Mega

import           Language.Ninja.Misc.Path (Path)

import qualified Control.Lens             as Lens

import           Flow                     ((.>), (|>))

--------------------------------------------------------------------------------

-- | Ninja top-level metadata, as documented
--   <https://ninja-build.org/manual.html#ref_toplevel here>.
--
--   @since 0.1.0
data Meta
  = MkMeta
    { _metaReqVersion :: !(Maybe Ver.Version)
    , _metaBuildDir   :: !(Maybe Path)
    }
  deriving (Eq, Ord, Show, Generic)

-- | Construct a default 'Meta' value.
--
--   @since 0.1.0
{-# INLINE makeMeta #-}
makeMeta :: Meta
makeMeta = MkMeta
           { _metaReqVersion = Nothing
           , _metaBuildDir   = Nothing
           }

-- | Corresponds to the @ninja_required_version@ top-level variable.
--
--   @since 0.1.0
{-# INLINE metaReqVersion #-}
metaReqVersion :: Lens.Lens' Meta (Maybe Ver.Version)
metaReqVersion = Lens.lens _metaReqVersion
                 $ \(MkMeta {..}) x -> MkMeta { _metaReqVersion = x, .. }

-- | Corresponds to the @builddir@ top-level variable.
--
--   @since 0.1.0
{-# INLINE metaBuildDir #-}
metaBuildDir :: Lens.Lens' Meta (Maybe Path)
metaBuildDir = Lens.lens _metaBuildDir
               $ \(MkMeta {..}) x -> MkMeta { _metaBuildDir = x, .. }

-- | Converts to @{req-version: …, build-dir: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Meta where
  toJSON (MkMeta {..})
    = [ "req-version" .= fmap versionJ _metaReqVersion
      , "build-dir"   .= _metaBuildDir
      ] |> Aeson.object
    where
      versionJ :: Ver.Version -> Aeson.Value
      versionJ = Ver.prettyVer .> Aeson.toJSON

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Meta where
  parseJSON = (Aeson.withObject "Meta" $ \o -> do
                  _metaReqVersion <- (o .: "req-version") >>= maybeVersionP
                  _metaBuildDir   <- (o .: "build-dir")   >>= pure
                  pure (MkMeta {..}))
    where
      maybeVersionP :: Maybe Aeson.Value -> Aeson.Parser (Maybe Ver.Version)
      maybeVersionP = fmap versionP .> sequenceA

      versionP :: Aeson.Value -> Aeson.Parser Ver.Version
      versionP = Aeson.withText "Version" (megaparsecToAeson Ver.version')

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable Meta

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Meta

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.Serial m Ver.Version
         , SC.Serial m Text
         ) => SC.Serial m Meta

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m
         , SC.CoSerial m Ver.Version
         , SC.CoSerial m Text
         ) => SC.CoSerial m Meta

--------------------------------------------------------------------------------

-- This function converts a @megaparsec@ parser to an @aeson@ parser.
-- Mainly, it handles converting the error output from @megaparsec@ to a
-- string that is appropriate for 'fail'.
megaparsecToAeson :: Mega.Parsec Mega.Dec Text t
                  -> (Text -> Aeson.Parser t)
megaparsecToAeson parser text = case Mega.runParser parser "" text of
                                  Left  e -> fail (Mega.parseErrorPretty e)
                                  Right x -> pure x

--------------------------------------------------------------------------------
