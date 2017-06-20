-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Internal/Str0.hs
--
-- License:
--     Copyright Neil Mitchell 2011-2017.
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions are
--     met:
--
--         * Redistributions of source code must retain the above copyright
--           notice, this list of conditions and the following disclaimer.
--
--         * Redistributions in binary form must reproduce the above
--           copyright notice, this list of conditions and the following
--           disclaimer in the documentation and/or other materials provided
--           with the distribution.
--
--         * Neither the name of Neil Mitchell nor the names of other
--           contributors may be used to endorse or promote products derived
--           from this software without specific prior written permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK #-}

-- |
--   Module      : Language.Ninja.Internal.Str0
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Internal.Str0
  ( module Language.Ninja.Internal.Str0 -- FIXME: specific export list
  ) where

import           Control.Applicative

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BSC8
import qualified Data.ByteString.Internal as BS.Internal
import qualified Data.ByteString.Unsafe   as BS.Unsafe

import           Data.Char
import           Data.Tuple.Extra
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Exts
import           Prelude
import           System.IO.Unsafe

--------------------------------------------------------------------------------

-- | A null-terminated strict bytestring.
newtype Str0
  = MkStr0 ByteString

type S = Ptr Word8

-- | FIXME: doc
char :: S -> Char
char x = BS.Internal.w2c $ unsafePerformIO $ peek x

next :: S -> S
next x = x `plusPtr` 1

{-# INLINE dropWhile0 #-}
dropWhile0 :: (Char -> Bool) -> Str0 -> Str0
dropWhile0 f x = snd $ span0 f x

{-# INLINE span0 #-}
span0 :: (Char -> Bool) -> Str0 -> (ByteString, Str0)
span0 f = break0 (not . f)

{-# INLINE break0 #-}
break0 :: (Char -> Bool) -> Str0 -> (ByteString, Str0)
break0 f (MkStr0 bs) = (initial, rest)
  where
    initial = BS.Unsafe.unsafeTake i bs
    rest    = MkStr0 (BS.Unsafe.unsafeDrop i bs)

    i = unsafePerformIO $ BS.Unsafe.unsafeUseAsCString bs $ \ptr -> do
      let start = castPtr ptr :: S
      let end = go start
      pure $! Ptr end `minusPtr` start

    go s@(Ptr a) | ((c == '\0') || (f c)) = a
                 | otherwise              = go (next s)
      where
        c = char s

{-# INLINE break00 #-}
-- The predicate must return true for '\0'
break00 :: (Char -> Bool) -> Str0 -> (ByteString, Str0)
break00 f (MkStr0 bs) = (initial, rest)
  where
    initial = BS.Unsafe.unsafeTake i bs
    rest    = MkStr0 $ BS.Unsafe.unsafeDrop i bs

    i = unsafePerformIO $ BS.Unsafe.unsafeUseAsCString bs $ \ptr -> do
      let start = castPtr ptr :: S
      let end = go start
      pure $! Ptr end `minusPtr` start

    go s@(Ptr a) | f c       = a
                 | otherwise = go (next s)
      where
        c = char s

head0 :: Str0 -> Char
head0 (MkStr0 x) = BS.Internal.w2c $ BS.Unsafe.unsafeHead x

tail0 :: Str0 -> Str0
tail0 (MkStr0 x) = MkStr0 $ BS.Unsafe.unsafeTail x

list0 :: Str0 -> (Char, Str0)
list0 x = (head0 x, tail0 x)

take0 :: Int -> Str0 -> ByteString
take0 i (MkStr0 x) = BSC8.takeWhile (/= '\0') $ BSC8.take i x

--------------------------------------------------------------------------------
