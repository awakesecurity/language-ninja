{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | FIXME: doc
module Language.Ninja.Pretty
  ( module Language.Ninja.Pretty -- FIXME: specific export list
  ) where

import           Language.Ninja.Types

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (unlines, unwords)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

prettyNinja :: Ninja -> ByteString
prettyNinja ninja
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettyRule :: (Str, Rule) -> ByteString
prettyRule (name, (MkRule {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettySingle :: (FileStr, Build) -> ByteString
prettySingle (file, (MkBuild {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
    ]

prettyMultiple :: ([FileStr], Build) -> ByteString
prettyMultiple (files, (MkBuild {..}))
  = BS.unlines $ map mconcat
    [ -- FIXME: finish implementing
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
