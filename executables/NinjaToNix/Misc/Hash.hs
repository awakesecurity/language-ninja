-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix/Misc/Hash.hs
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

{-# LANGUAGE PackageImports #-}

-- |
--   Module      : NinjaToNix.Misc.Hash
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module NinjaToNix.Misc.Hash
  ( sha256
  , module Exported
  ) where

import           "cryptonite" Crypto.Hash     as Exported

import           Data.ByteString (ByteString)

import           Data.Text       (Text)
import qualified Data.Text       as Text

digestSHA256 :: ByteString -> Digest SHA256
digestSHA256 = hash

-- | FIXME: doc
sha256 :: ByteString -> Text
sha256 = Text.pack . show . digestSHA256
