{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | FIXME: doc
module Ninja.Pretty
  ( module Ninja.Pretty -- FIXME: specific export list
  ) where

import           Ninja.Type

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (unlines, unwords)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

prettyNinja :: Ninja -> ByteString
prettyNinja ninja
  = BS.unlines $ map mconcat
    [
    ]

prettyRule :: (Str, Rule) -> ByteString
prettyRule (name, (MkRule {..}))
  = BS.unlines $ map mconcat
    [
    ]

prettySingle :: (FileStr, Build) -> ByteString
prettySingle (file, (MkBuild {..}))
  = BS.unlines $ map mconcat
    [
    ]

prettyMultiple :: ([FileStr], Build) -> ByteString
prettyMultiple (files, (MkBuild {..}))
  = BS.unlines $ map mconcat
    [
    ]

prettyPhony :: (Str, [FileStr]) -> ByteString
prettyPhony (name, files)
  = BS.unlines $ map mconcat
    [ ["build ", name, ": phony ", BS.unwords files]
    ]

prettyDefault :: FileStr -> ByteString
prettyDefault def
  = BS.unlines $ map mconcat
    [ ["default ", def]
    ]

prettyPool :: (Str, Int) -> ByteString
prettyPool (name, depth)
  = BS.unlines $ map mconcat
    [ ["pool ", name]
    , [" depth = ", bshow depth]
    ]

bshow :: (Show s) => s -> Str
bshow = T.encodeUtf8 . T.pack . show
