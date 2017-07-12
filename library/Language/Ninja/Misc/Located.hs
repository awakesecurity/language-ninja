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

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
--
--   @since 0.1.0
module Language.Ninja.Misc.Located
  ( -- * @Located@
    Located
  , tokenize, tokenizeFile, tokenizeText
  , untokenize
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

import           Control.Monad.ST         (ST)
import qualified Control.Monad.ST         as ST
import           Data.STRef               (STRef)
import qualified Data.STRef               as ST

import           Data.Char                (isSpace)
import qualified Data.Maybe
import           Data.Semigroup           (Semigroup (..))

import           Data.Text                (Text)
import qualified Data.Text                as Text

import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

import           Data.Map.Strict          (Map)

import           Control.DeepSeq          (NFData)
import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)

import qualified Test.SmallCheck.Series   as SC

import           Data.Aeson               ((.:), (.=))
import qualified Data.Aeson               as Aeson

import           Flow                     ((.>), (|>))

import qualified Language.Ninja.Misc.Path as Misc
import qualified Language.Ninja.Mock      as Mock

--------------------------------------------------------------------------------

-- | This datatype represents a value annotated with a source location.
--
--   @since 0.1.0
data Located t
  = MkLocated
    { _locatedPos :: {-# UNPACK #-} !Position
    , _locatedVal ::                !t
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Construct a 'Located' value directly.
--
--   @since 0.1.0
{-# INLINE makeLocated #-}
makeLocated :: Position -> t -> Located t
makeLocated = MkLocated

-- | Given @path :: 'Maybe' 'Misc.Path'@ and a @text :: 'Text'@, do the
--   following:
--
--   * Remove all @'\r'@ characters from the @text@.
--   * Split the @text@ into chunks that are guaranteed not to contain newlines
--     or whitespace, and which are annotated with their location.
--
--   @since 0.1.0
tokenize :: Maybe Misc.Path -> Text -> [Located Text]
tokenize mpath = removeWhitespace (mpath, 0, 0)

-- | Read the file at the given 'Misc.Path' and then run 'tokenize' on the
--   resulting 'Text'.
--
--   @since 0.1.0
tokenizeFile :: (Mock.MonadReadFile m) => Misc.Path -> m [Located Text]
tokenizeFile path = tokenize (Just path) <$> Mock.readFile path

-- | This function is equivalent to @tokenize Nothing@.
--
--   @since 0.1.0
tokenizeText :: Text -> [Located Text]
tokenizeText = tokenize Nothing

-- | FIXME: doc
--
--   FIXME: implement
--
--   @since 0.1.0
untokenize :: [Located Text] -> Map Misc.Path Text
untokenize = error "Language.Ninja.Misc.Located.untokenize is not yet written"

-- | The position of this located value.
--
--   @since 0.1.0
{-# INLINE locatedPos #-}
locatedPos :: Lens.Lens' (Located t) Position
locatedPos = Lens.lens _locatedPos
             $ \(MkLocated {..}) x -> MkLocated { _locatedPos = x, .. }

-- | The value underlying this located value.
--
--   @since 0.1.0
{-# INLINE locatedVal #-}
locatedVal :: Lens.Lens' (Located t) t
locatedVal = Lens.lens _locatedVal
             $ \(MkLocated {..}) x -> MkLocated { _locatedVal = x, .. }

-- | Converts to @{position: …, value: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON t) => Aeson.ToJSON (Located t) where
  toJSON (MkLocated {..})
    = [ "pos" .= _locatedPos
      , "val" .= _locatedVal
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON t) => Aeson.FromJSON (Located t) where
  parseJSON = (Aeson.withObject "Located" $ \o -> do
                  _locatedPos <- (o .: "pos") >>= pure
                  _locatedVal <- (o .: "val") >>= pure
                  pure (MkLocated {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable t) => Hashable (Located t)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData t) => NFData (Located t)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, SC.Serial m Text, SC.Serial m t
         ) => SC.Serial m (Located t)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, SC.CoSerial m Text, SC.CoSerial m t
         ) => SC.CoSerial m (Located t)

--------------------------------------------------------------------------------

-- | FIXME: doc
--
--   @since 0.1.0
newtype Spans
  = MkSpans (HashSet Span)
  deriving ( Eq, Show, Semigroup, Monoid
           , Generic, Aeson.ToJSON, Aeson.FromJSON
           , Hashable, NFData )

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE makeSpans #-}
makeSpans :: [Span] -> Spans
makeSpans = HS.fromList .> MkSpans

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE spansSet #-}
spansSet :: Lens.Iso' Spans (HashSet Span)
spansSet = Lens.iso (\(MkSpans s) -> s) MkSpans

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m (HashSet Span)) => SC.Serial m Spans

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m (HashSet Span)) => SC.CoSerial m Spans

--------------------------------------------------------------------------------

-- | Represents a span of source code.
--
--   @since 0.1.0
data Span
  = MkSpan !(Maybe Misc.Path) !Offset !Offset
  deriving (Eq, Show, Generic)

-- | Construct a 'Span' from a given start position to a given end position.
--
--   @since 0.1.0
{-# INLINE makeSpan #-}
makeSpan :: Maybe Misc.Path
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
--
--   @since 0.1.0
{-# INLINE spanPath #-}
spanPath :: Lens.Lens' Span (Maybe Misc.Path)
spanPath = let helper (MkSpan p s e) = (p, \x -> MkSpan x s e)
           in Lens.lens (helper .> fst) (helper .> snd)

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE spanRange #-}
spanRange :: Lens.Lens' Span (Offset, Offset)
spanRange = let helper (MkSpan p s e) = ((s, e), \(s', e') -> MkSpan p s' e')
            in Lens.lens (helper .> fst) (helper .> snd)

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE spanStart #-}
spanStart :: Lens.Lens' Span Offset
spanStart = spanRange . Lens._1

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE spanEnd #-}
spanEnd :: Lens.Lens' Span Offset
spanEnd = spanRange . Lens._2

-- | Converts to @{file: …, start: …, end: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Span where
  toJSON (MkSpan file start end)
    = [ "file"  .= maybe Aeson.Null Aeson.toJSON file
      , "start" .= offsetJ start
      , "end"   .= offsetJ end
      ] |> Aeson.object
    where
      offsetJ (line, col) = Aeson.object ["line" .= line, "col" .= col]

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Span where
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
--
--   @since 0.1.0
instance Hashable Span

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Span

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Span

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Span

--------------------------------------------------------------------------------

-- | This datatype represents the position of a cursor in a text file.
--
--   @since 0.1.0
data Position
  = MkPosition
    { _positionFile ::                !(Maybe Misc.Path)
    , _positionLine :: {-# UNPACK #-} !Line
    , _positionCol  :: {-# UNPACK #-} !Column
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Position' from a (nullable) path and a @(line, column)@ pair.
--
--   @since 0.1.0
{-# INLINE makePosition #-}
makePosition :: Maybe Misc.Path -> Offset -> Position
makePosition file (line, column) = MkPosition file line column

-- | The path of the file pointed to by this position, if any.
--
--   @since 0.1.0
{-# INLINE positionFile #-}
positionFile :: Lens.Lens' Position (Maybe Misc.Path)
positionFile = Lens.lens _positionFile
               $ \(MkPosition {..}) x -> MkPosition { _positionFile = x, .. }

-- | The offset in the file pointed to by this position.
--
--   @since 0.1.0
{-# INLINE positionOffset #-}
positionOffset :: Lens.Lens' Position Offset
positionOffset
  = Lens.lens (_positionLine &&& _positionCol)
    $ \(MkPosition {..}) (line, col) ->
        MkPosition { _positionLine = line, _positionCol = col, .. }

-- | The line number in the file pointed to by this position.
--
--   @since 0.1.0
{-# INLINE positionLine #-}
positionLine :: Lens.Lens' Position Line
positionLine = positionOffset . Lens._1

-- | The column number in the line pointed to by this position.
--
--   @since 0.1.0
{-# INLINE positionCol #-}
positionCol :: Lens.Lens' Position Column
positionCol = positionOffset . Lens._2

-- | FIXME: doc
--
--   @since 0.1.0
comparePosition :: Position -> Position -> Maybe Ordering
comparePosition = go
  where
    go (MkPosition fileX lineX colX) (MkPosition fileY lineY colY)
      = compareTriple (fileX, (lineX, colX)) (fileY, (lineY, colY))

    compareTriple (mfileX, offX) (mfileY, offY)
      | (mfileX == mfileY) = Just (compareOffset offX offY)
      | otherwise          = Nothing

-- | Converts to @{file: …, line: …, col: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Position where
  toJSON (MkPosition {..})
    = [ "file" .= _positionFile
      , "line" .= _positionLine
      , "col"  .= _positionCol
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Position where
  parseJSON = (Aeson.withObject "Position" $ \o -> do
                  _positionFile <- (o .: "file") >>= pure
                  _positionLine <- (o .: "line") >>= pure
                  _positionCol  <- (o .: "col")  >>= pure
                  pure (MkPosition {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable Position

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Position

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Position

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Position

--------------------------------------------------------------------------------

-- | A line/column offset into a file.
--
--   @since 0.1.0
type Offset = (Line, Column)

-- | FIXME: doc
--
--   @since 0.1.0
compareOffset :: Offset -> Offset -> Ordering
compareOffset (lineX, colX) (lineY, colY)
  | (lineX < lineY) = LT
  | (lineX > lineY) = GT
  | otherwise       = compare colX colY

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE offsetLine #-}
offsetLine :: Lens.Lens' Offset Line
offsetLine = Lens._1

-- | FIXME: doc
--
--   @since 0.1.0
{-# INLINE offsetColumn #-}
offsetColumn :: Lens.Lens' Offset Column
offsetColumn = Lens._2

--------------------------------------------------------------------------------

-- | A line number.
--
--   @since 0.1.0
type Line = Int

-- | A column number.
--
--   @since 0.1.0
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
chunksAddChar c                = chunksCons (ChunkText (Text.singleton c))

--------------------------------------------------------------------------------

removeWhitespace :: (Maybe Misc.Path, Line, Column) -> Text -> [Located Text]
removeWhitespace (file, initLine, initCol) =
  go .> Data.Maybe.catMaybes .> map makeLoc
  where
    go :: Text -> [Maybe (Line, Column, Text)]
    go text = ST.runST $ do
      ref <- ST.newSTRef (initLine, initCol)
      Text.foldr chunksAddChar chunksNil text
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
                         ST.modifySTRef' ref (second (+ Text.length t))
                         pure (Just (line, column, t))

    {-# INLINE makeLoc #-}
    makeLoc :: (Line, Column, Text) -> Located Text
    makeLoc (line, col, text) = makeLocated (MkPosition file line col) text

--------------------------------------------------------------------------------
