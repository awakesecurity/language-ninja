{-# LANGUAGE PackageImports #-}

module Hash
  ( sha256
  , module Exported
  ) where

import           "cryptonite" Crypto.Hash     as Exported

import qualified Data.ByteString as BS

import qualified Data.Text       as T

digestSHA256 :: BS.ByteString -> Digest SHA256
digestSHA256 = hash

sha256 :: BS.ByteString -> T.Text
sha256 = T.pack . show . digestSHA256
