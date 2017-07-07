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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

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

    -- * @Spans@
  , Spans, makeSpans, spansSet

    -- * @Span@
  , Span, makeSpan, spanPath, spanRange, spanStart, spanEnd

    -- * @Position@
  , Position, makePosition
  , positionFile, positionOffset, positionLine, positionCol
  , comparePosition

    -- * @Offset@
  , Offset, compareOffset, offsetLine, offsetColumn

    -- * Miscellaneous
  , Line, Column
  ) where

import           Control.Arrow            (second, (&&&), (***))

import qualified Control.Lens             as Lens
import           Control.Lens.Getter      ((^.))
import           Control.Lens.Lens        (Lens')
import           Control.Lens.Tuple       (_1, _2)

import           Control.Monad.ST         (ST)
import qualified Control.Monad.ST         as ST
import           Data.STRef               (STRef)
import qualified Data.STRef               as ST

import           Data.Char                (isSpace)
import qualified Data.Maybe
import           Data.Semigroup           (Semigroup (..))

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import qualified Data.List.NonEmpty       as NE

import           Control.DeepSeq          (NFData)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)

import qualified Test.SmallCheck.Series   as SC

import           Data.Aeson
                 (FromJSON (..), KeyValue (..), ToJSON (..), (.:))
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Types         as Aeson

import           Flow                     ((.>), (|>))

import           Language.Ninja.Misc.Path (Path)
import qualified Language.Ninja.Misc.Path as Ninja

--------------------------------------------------------------------------------

-- | This datatype represents a value annotated with a source location.
data Located t
  = MkLocated
    { _locatedPos :: {-# UNPACK #-} !Position
    , _locatedVal ::                !t
    }
  deriving (Eq, Show, Functor, Generic)

-- | Construct a 'Located' value directly.
{-# INLINE makeLocated #-}
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
{-# INLINE locatedPos #-}
locatedPos :: Lens' (Located t) Position
locatedPos = Lens.lens _locatedPos
             $ \(MkLocated {..}) x -> MkLocated { _locatedPos = x, .. }

-- | The value underlying this located value.
{-# INLINE locatedVal #-}
locatedVal :: Lens' (Located t) t
locatedVal = Lens.lens _locatedVal
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

-- | Default 'Hashable' instance via 'Generic'.
instance (Hashable t) => Hashable (Located t)

-- | Default 'NFData' instance via 'Generic'.
instance (NFData t) => NFData (Located t)

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, SC.Serial m Text, SC.Serial m t
         ) => SC.Serial m (Located t)

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, SC.CoSerial m Text, SC.CoSerial m t
         ) => SC.CoSerial m (Located t)

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Spans
  = MkSpans (HashSet Span)
  deriving ( Eq, Show, Semigroup, Monoid
           , Generic, ToJSON, FromJSON
           , Hashable, NFData )

-- | FIXME: doc
{-# INLINE makeSpans #-}
makeSpans :: [Span] -> Spans
makeSpans = HS.fromList .> MkSpans

-- | FIXME: doc
{-# INLINE spansSet #-}
spansSet :: Lens.Iso' Spans (HashSet Span)
spansSet = Lens.iso (\(MkSpans s) -> s) MkSpans

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, SC.Serial m (HashSet Span)) => SC.Serial m Spans

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, SC.CoSerial m (HashSet Span)) => SC.CoSerial m Spans

--------------------------------------------------------------------------------

-- | Represents a span of source code.
data Span
  = MkSpan !(Maybe Path) !Offset !Offset
  deriving (Eq, Show, Generic)

-- | Construct a 'Span' from a given start position to a given end position.
{-# INLINE makeSpan #-}
makeSpan :: Maybe Path
         -- ^ The file in which this span resides, if any.
         -> Offset
         -- ^ The start offset.
         -> Offset
         -- ^ The end offset.
         -> Span
makeSpan mpath start end = case compareOffset start end of
                             GT -> makeSpan mpath end start
                             _  -> MkSpan mpath start end

-- | FIXME: doc
{-# INLINE spanPath #-}
spanPath :: Lens.Lens' Span (Maybe Path)
spanPath = let helper (MkSpan p s e) = (p, \x -> MkSpan x s e)
           in Lens.lens (helper .> fst) (helper .> snd)

-- | FIXME: doc
{-# INLINE spanRange #-}
spanRange :: Lens.Lens' Span (Offset, Offset)
spanRange = let helper (MkSpan p s e) = ((s, e), \(s', e') -> MkSpan p s' e')
            in Lens.lens (helper .> fst) (helper .> snd)

-- | FIXME: doc
{-# INLINE spanStart #-}
spanStart :: Lens.Lens' Span Offset
spanStart = spanRange . _1

-- | FIXME: doc
{-# INLINE spanEnd #-}
spanEnd :: Lens.Lens' Span Offset
spanEnd = spanRange . _2

-- | Converts to @{file: …, start: …, end: …}@.
instance ToJSON Span where
  toJSON (MkSpan file start end)
    = [ "file"  .= maybe Aeson.Null toJSON file
      , "start" .= offsetJ start
      , "end"   .= offsetJ end
      ] |> Aeson.object
    where
      offsetJ (line, col) = Aeson.object ["line" .= line, "col" .= col]

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Span where
  parseJSON = (Aeson.withObject "Span" $ \o -> do
                  file  <- (o .: "file")  >>= pure
                  start <- (o .: "start") >>= offsetP
                  end   <- (o .: "end")   >>= offsetP
                  pure (MkSpan file start end))
    where
      offsetP = (Aeson.withObject "Offset" $ \o -> do
                    line <- (o .: "line") >>= pure
                    col  <- (o .: "col")  >>= pure
                    pure (line, col))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Span

-- | Default 'NFData' instance via 'Generic'.
instance NFData Span

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, SC.Serial m Text) => SC.Serial m Span

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Span

--------------------------------------------------------------------------------

-- | This datatype represents position of a cursor
data Position
  = MkPosition
    { _positionFile ::                !(Maybe Path)
    , _positionLine :: {-# UNPACK #-} !Line
    , _positionCol  :: {-# UNPACK #-} !Column
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Position' from a (nullable) path and a @(line, column)@ pair.
{-# INLINE makePosition #-}
makePosition :: Maybe Path -> Offset -> Position
makePosition file (line, column) = MkPosition file line column

-- | The path of the file pointed to by this position, if any.
{-# INLINE positionFile #-}
positionFile :: Lens' Position (Maybe Path)
positionFile = Lens.lens _positionFile
               $ \(MkPosition {..}) x -> MkPosition { _positionFile = x, .. }

-- | The offset in the file pointed to by this position.
{-# INLINE positionOffset #-}
positionOffset :: Lens' Position Offset
positionOffset
  = Lens.lens (_positionLine &&& _positionCol)
    $ \(MkPosition {..}) (line, col) ->
        MkPosition { _positionLine = line, _positionCol = col, .. }

-- | The line number in the file pointed to by this position.
{-# INLINE positionLine #-}
positionLine :: Lens' Position Line
positionLine = positionOffset . _1

-- | The column number in the line pointed to by this position.
{-# INLINE positionCol #-}
positionCol :: Lens' Position Column
positionCol = positionOffset . _2

-- | FIXME: doc
comparePosition :: Position -> Position -> Maybe Ordering
comparePosition = go
  where
    go (MkPosition fileX lineX colX) (MkPosition fileY lineY colY)
      = compareTriple (fileX, (lineX, colX)) (fileY, (lineY, colY))

    compareTriple (mfileX, offX) (mfileY, offY)
      | (mfileX == mfileY) = Just (compareOffset offX offY)
      | otherwise          = Nothing

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

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Position

-- | Default 'NFData' instance via 'Generic'.
instance NFData Position

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, SC.Serial m Text) => SC.Serial m Position

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Position

--------------------------------------------------------------------------------

-- | A line/column offset into a file.
type Offset = (Line, Column)

-- | FIXME: doc
compareOffset :: Offset -> Offset -> Ordering
compareOffset (lineX, colX) (lineY, colY)
  | (lineX < lineY) = LT
  | (lineX > lineY) = GT
  | otherwise       = compare colX colY

-- | FIXME: doc
{-# INLINE offsetLine #-}
offsetLine :: Lens.Lens' Offset Line
offsetLine = _1

-- | FIXME: doc
{-# INLINE offsetColumn #-}
offsetColumn :: Lens.Lens' Offset Column
offsetColumn = _2

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

{-# INLINE chunksNil #-}
chunksNil :: Chunks
chunksNil = MkChunks []

chunksCons :: Chunk -> Chunks -> Chunks
chunksCons = \chunk (MkChunks list) -> MkChunks (go chunk list)
  where
    {-# INLINE go #-}
    go (ChunkSpace m) (ChunkSpace n : rest) = ChunkSpace (m + n)  : rest
    go (ChunkLine  m) (ChunkLine  n : rest) = ChunkLine  (m + n)  : rest
    go (ChunkText  a) (ChunkText  b : rest) = ChunkText  (a <> b) : rest
    go other          list                  = other               : list

{-# INLINE chunksAddChar #-}
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
    go text = ST.runST $ do
      ref <- ST.newSTRef (initLine, initCol)
      T.foldr chunksAddChar chunksNil text
        |> fromChunks
        |> mapM (applyChunk ref)

    applyChunk :: STRef s (Line, Column)
               -> Chunk -> ST s (Maybe (Line, Column, Text))
    applyChunk ref = \case
      ChunkLine  n -> do ST.modifySTRef' ref ((+ n) *** const 0)
                         pure Nothing
      ChunkSpace n -> do ST.modifySTRef' ref (second (+ n))
                         pure Nothing
      ChunkText  t -> do (line, column) <- ST.readSTRef ref
                         ST.modifySTRef' ref (second (+ T.length t))
                         pure (Just (line, column, t))

    {-# INLINE makeLoc #-}
    makeLoc :: (Line, Column, Text) -> Located Text
    makeLoc (line, col, text) = makeLocated (MkPosition file line col) text

--------------------------------------------------------------------------------
