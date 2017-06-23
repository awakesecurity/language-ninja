-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc/Located.hs
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

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
--   Module      : Language.Ninja.Misc.Located
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Tokenize text into a list of non-whitespace chunks, each of which is
--   annotated with its source location.
module Language.Ninja.Misc.Located
  ( -- * @Located@
    Located, tokenize, tokenizeFile, tokenizeText
  , locatedPos, locatedVal

    -- * @Position@
  , Position, makePosition
  , positionFile, positionLine, positionCol

    -- * Miscellaneous
  , Line, Column
  ) where

import           Control.Arrow            (second, (***))

import           Control.Lens.Getter      ((^.))
import           Control.Lens.Lens        (Lens')
import qualified Control.Lens

import           Control.Monad.ST         (ST)
import qualified Control.Monad.ST
import           Data.STRef               (STRef)
import qualified Data.STRef

import           Data.Char                (isSpace)
import qualified Data.Maybe
import           Data.Monoid              ((<>))

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import           Data.Aeson               (FromJSON(..), KeyValue(..),
                                           ToJSON(..), (.:))
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Types         as Aeson

import           Flow                     ((|>), (.>))

import           Language.Ninja.Misc.Path (Path)
import qualified Language.Ninja.Misc.Path as Ninja

--------------------------------------------------------------------------------

-- | This datatype represents a value annotated with a source location.
data Located t
  = MkLocated
    { _locatedPos :: {-# UNPACK #-} !Position
    , _locatedVal ::                !t
    }
  deriving (Eq, Show, Functor)

-- | Construct a 'Located' value directly.
makeLocated :: Position -> t -> Located t
makeLocated = MkLocated

-- | Given @path :: Maybe Path@ and a @text :: Text@, do the following:
--   * Remove all @'\r'@ characters from the @text@.
--   * Split the @text@ into chunks that are guaranteed not to contain newlines
--     or whitespace, and which are annotated with their location.
tokenize :: Maybe Path -> Text -> [Located Text]
tokenize mpath = removeWhitespace (mpath, 0, 0)

-- | Read the file at the given 'Path' and then run 'tokenize' on the
--   resulting 'Text'.
tokenizeFile :: Path -> IO [Located Text]
tokenizeFile path = tokenize (Just path)
                    <$> T.readFile (T.unpack (path ^. Ninja.pathText))

-- | This function is equivalent to @tokenize Nothing@.
tokenizeText :: Text -> [Located Text]
tokenizeText = tokenize Nothing

-- | FIXME: doc
untokenize :: [Located Text] -> Map Path Text
untokenize = undefined -- FIXME: implement

-- | The position of this located value.
locatedPos :: Lens' (Located t) Position
locatedPos = Control.Lens.lens _locatedPos
             $ \(MkLocated {..}) x -> MkLocated { _locatedPos = x, .. }

-- | The value underlying this located value.
locatedVal :: Lens' (Located t) t
locatedVal = Control.Lens.lens _locatedVal
             $ \(MkLocated {..}) x -> MkLocated { _locatedVal = x, .. }

-- | Converts to @{position: …, value: …}@.
instance (ToJSON t) => ToJSON (Located t) where
  toJSON (MkLocated {..})
    = [ "pos" .= _locatedPos
      , "val" .= _locatedVal
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance (FromJSON t) => FromJSON (Located t) where
  parseJSON = (Aeson.withObject "Located" $ \o -> do
                  _locatedPos <- (o .: "pos") >>= pure
                  _locatedVal <- (o .: "val") >>= pure
                  pure (MkLocated {..}))

--------------------------------------------------------------------------------

-- | This datatype represents position of a cursor
data Position
  = MkPosition
    { _positionFile ::                !(Maybe Path)
    , _positionLine :: {-# UNPACK #-} !Line
    , _positionCol  :: {-# UNPACK #-} !Column
    }
  deriving (Eq, Show)

-- | Construct a 'Position' from a (nullable) path and a @(line, column)@ pair.
makePosition :: Maybe Path -> (Line, Column) -> Position
makePosition file (line, column) = MkPosition file line column

-- | The path of the file pointed to by this position, if any.
positionFile :: Lens' Position (Maybe Path)
positionFile = Control.Lens.lens _positionFile
               $ \(MkPosition {..}) x -> MkPosition { _positionFile = x, .. }

-- | The line number in the file pointed to by this position.
positionLine :: Lens' Position Line
positionLine = Control.Lens.lens _positionLine
               $ \(MkPosition {..}) x -> MkPosition { _positionLine = x, .. }

-- | The column number in the line pointed to by this position.
positionCol :: Lens' Position Column
positionCol = Control.Lens.lens _positionCol
              $ \(MkPosition {..}) x -> MkPosition { _positionCol = x, .. }

-- | Converts to @{file: …, line: …, col: …}@.
instance ToJSON Position where
  toJSON (MkPosition {..})
    = [ "file" .= _positionFile
      , "line" .= _positionLine
      , "col"  .= _positionCol
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Position where
  parseJSON = (Aeson.withObject "Position" $ \o -> do
                  _positionFile <- (o .: "file") >>= pure
                  _positionLine <- (o .: "line") >>= pure
                  _positionCol  <- (o .: "col")  >>= pure
                  pure (MkPosition {..}))

--------------------------------------------------------------------------------

-- | A line number.
type Line = Int

-- | A column number.
type Column = Int

--------------------------------------------------------------------------------

data Chunk
  = ChunkText                 !Text
  | ChunkSpace {-# UNPACK #-} !Int
  | ChunkLine  {-# UNPACK #-} !Int
  deriving (Eq, Show)

--------------------------------------------------------------------------------

newtype Chunks
  = MkChunks { fromChunks :: [Chunk] }
  deriving (Eq, Show)

chunksNil :: Chunks
chunksNil = MkChunks []

chunksCons :: Chunk -> Chunks -> Chunks
chunksCons = \chunk (MkChunks list) -> MkChunks (go chunk list)
  where
    go (ChunkSpace m) (ChunkSpace n : rest) = ChunkSpace (m + n)  : rest
    go (ChunkLine  m) (ChunkLine  n : rest) = ChunkLine  (m + n)  : rest
    go (ChunkText  a) (ChunkText  b : rest) = ChunkText  (a <> b) : rest
    go other          list                  = other               : list

chunksAddChar :: Char -> Chunks -> Chunks
chunksAddChar '\n'             = chunksCons (ChunkLine 1)
chunksAddChar '\r'             = id
chunksAddChar c    | isSpace c = chunksCons (ChunkSpace 1)
chunksAddChar c                = chunksCons (ChunkText (T.singleton c))

--------------------------------------------------------------------------------

removeWhitespace :: (Maybe Path, Line, Column) -> Text -> [Located Text]
removeWhitespace (file, initLine, initCol) =
  go .> Data.Maybe.catMaybes .> map makeLoc
  where
    go :: Text -> [Maybe (Line, Column, Text)]
    go text = Control.Monad.ST.runST $ do
      ref <- Data.STRef.newSTRef (initLine, initCol)
      T.foldr chunksAddChar chunksNil text
        |> fromChunks
        |> mapM (applyChunk ref)

    applyChunk :: STRef s (Line, Column)
               -> Chunk -> ST s (Maybe (Line, Column, Text))
    applyChunk ref = \case
      ChunkLine  n -> do Data.STRef.modifySTRef' ref ((+ n) *** const 0)
                         pure Nothing
      ChunkSpace n -> do Data.STRef.modifySTRef' ref (second (+ n))
                         pure Nothing
      ChunkText  t -> do (line, column) <- Data.STRef.readSTRef ref
                         Data.STRef.modifySTRef' ref (second (+ T.length t))
                         pure (Just (line, column, t))

    makeLoc :: (Line, Column, Text) -> Located Text
    makeLoc (line, col, text) = makeLocated (MkPosition file line col) text

--------------------------------------------------------------------------------
