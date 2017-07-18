-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Lexer/Types.hs
--
-- License:
--     Copyright Neil Mitchell  2011-2017.
--     Copyright Awake Networks 2017.
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

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.Lexer
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Lexing is a slow point, the code below is optimised.
--
--   @since 0.1.0
module Language.Ninja.Lexer.Types
  ( -- * Type aliases
    Parser, Ann

    -- * @Lexeme@ and friends
  , Lexeme (..)
  , LName  (..)
  , LFile  (..)
  , LBind  (..)
  , LBuild (..), makeLBuild

  , LexemeConstraint
  , LNameConstraint
  , LFileConstraint
  , LBindConstraint
  , LBuildConstraint

    -- * @PositionParsing@
  , PositionParsing (..)
  , spanned
  ) where

import           Control.Arrow          (second)
import           Control.Monad          (when)

import qualified Control.Lens           as Lens

import           Data.ByteString        (ByteString)

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text

import           Flow                   ((.>), (|>))

import qualified Text.Megaparsec        as M

import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Types       as Aeson

import           Control.DeepSeq        (NFData)
import           Data.Hashable          (Hashable)
import           GHC.Generics           (Generic)

import           Test.SmallCheck.Series ((<~>))
import qualified Test.SmallCheck.Series as SC

import           GHC.Exts               (Constraint)

import qualified Language.Ninja.AST     as AST
import qualified Language.Ninja.Misc    as Misc

--------------------------------------------------------------------------------

-- | A @megaparsec@ parser.
--
--   @since 0.1.0
type Parser m a = M.ParsecT M.Dec Text m a

-- | The type of annotations returned by the lexer.
--
--   @since 0.1.0
type Ann = Misc.Spans

--------------------------------------------------------------------------------

-- | This class is kind of like 'DeltaParsing' from @trifecta@.
--
--   @since 0.1.0
class (Monad m) => PositionParsing m where
  getPosition :: m Misc.Position

-- | Instance for 'M.ParsecT' from @megaparsec@.
--
--   @since 0.1.0
instance PositionParsing (M.ParsecT M.Dec Text m) where
  getPosition = convert <$> M.getPosition
    where
      convert :: M.SourcePos -> Misc.Position
      convert (M.SourcePos fp line column)
        = let path = Lens.view (Lens.from Misc.pathString) fp
          in Misc.makePosition (Just path) (toLine line, toColumn column)

      toLine   :: M.Pos -> Misc.Line
      toColumn :: M.Pos -> Misc.Column
      toLine   = M.unPos .> fromIntegral
      toColumn = M.unPos .> fromIntegral

-- | Surround a section of parsers in 'getPosition' calls and return the
--   associated 'Misc.Spans'. Note that if a call of this function wraps over
--   a parser that somehow goes over multiple files, it will 'fail'.
--
--   @since 0.1.0
spanned :: (PositionParsing m) => m a -> m (Misc.Spans, a)
spanned p = do
  start  <- getPosition
  result <- p
  end    <- getPosition
  let getPosFile :: Misc.Position -> Maybe Misc.Path
      getPosFile = Lens.view Misc.positionFile
  let (sfile, efile) = (getPosFile start, getPosFile end)
  when (sfile /= efile) $ fail "spanned: somehow went over multiple files!"
  let file = sfile
  let offS = Lens.view Misc.positionOffset start
  let offE = Lens.view Misc.positionOffset end
  pure (Misc.makeSpans [Misc.makeSpan file offS offE], result)

--------------------------------------------------------------------------------

-- | Lex each line separately, rather than each lexeme.
--
--   @since 0.1.0
data Lexeme ann
  = -- | @foo = bar@
    --
    --   @since 0.1.0
    LexDefine   !ann !(LBind ann)
  | -- | @[indent]foo = bar@
    --
    --   @since 0.1.0
    LexBind     !ann !(LBind ann)
  | -- | @include file@
    --
    --   @since 0.1.0
    LexInclude  !ann !(LFile ann)
  | -- | @subninja file@
    --
    --   @since 0.1.0
    LexSubninja !ann !(LFile ann)
  | -- | @build foo: bar | baz || qux@
    --
    --   @since 0.1.0
    LexBuild    !ann !(LBuild ann)
  | -- | @rule name@
    --
    --   @since 0.1.0
    LexRule     !ann !(LName ann)
  | -- | @pool name@
    --
    --   @since 0.1.0
    LexPool     !ann !(LName ann)
  | -- | @default foo bar@
    --
    --   @since 0.1.0
    LexDefault  !ann ![AST.Expr ann]
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated Lexeme where
  annotation' f = Lens.lens (helper .> fst) (helper .> snd)
    where
      helper (LexDefine   ann v) = (ann, \x -> LexDefine   x (f <$> v))
      helper (LexBind     ann v) = (ann, \x -> LexBind     x (f <$> v))
      helper (LexInclude  ann v) = (ann, \x -> LexInclude  x (f <$> v))
      helper (LexSubninja ann v) = (ann, \x -> LexSubninja x (f <$> v))
      helper (LexBuild    ann v) = (ann, \x -> LexBuild    x (f <$> v))
      helper (LexRule     ann v) = (ann, \x -> LexRule     x (f <$> v))
      helper (LexPool     ann v) = (ann, \x -> LexPool     x (f <$> v))
      helper (LexDefault  ann v) = (ann, \x -> LexDefault  x (map (fmap f) v))

-- | Converts to @{ann: …, tag: …, value: …}@.
--
--   @since 0.1.0
instance forall ann. (Aeson.ToJSON ann) => Aeson.ToJSON (Lexeme ann) where
  toJSON = (\case (LexDefine   ann value) -> obj ann "define"   value
                  (LexBind     ann value) -> obj ann "bind"     value
                  (LexInclude  ann value) -> obj ann "include"  value
                  (LexSubninja ann value) -> obj ann "subninja" value
                  (LexBuild    ann value) -> obj ann "build"    value
                  (LexRule     ann value) -> obj ann "rule"     value
                  (LexPool     ann value) -> obj ann "pool"     value
                  (LexDefault  ann value) -> obj ann "default"  value)
    where
      obj :: forall x. (Aeson.ToJSON x) => ann -> Text -> x -> Aeson.Value
      obj ann tag value = [ "ann" .= ann, "tag" .= tag, "value" .= value
                          ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (Lexeme ann) where
  parseJSON = (Aeson.withObject "Lexeme" $ \o -> do
                  ann <- o .: "ann"
                  tag <- o .: "tag"
                  case (tag :: Text) of
                    "define"   -> LexDefine   ann <$> (o .: "value")
                    "bind"     -> LexBind     ann <$> (o .: "value")
                    "include"  -> LexInclude  ann <$> (o .: "value")
                    "subninja" -> LexSubninja ann <$> (o .: "value")
                    "build"    -> LexBuild    ann <$> (o .: "value")
                    "rule"     -> LexRule     ann <$> (o .: "value")
                    "pool"     -> LexPool     ann <$> (o .: "value")
                    "default"  -> LexDefault  ann <$> (o .: "value")
                    owise      -> invalidTagError (Text.unpack owise))
    where
      invalidTagError :: String -> Aeson.Parser a
      invalidTagError x = [ "Invalid tag: ", x, "; expected one of: "
                          , show validTags
                          ] |> mconcat |> fail

      validTags :: [Text]
      validTags = [ "define", "bind", "include", "subninja"
                  , "build", "rule", "pool", "default" ]

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (Lexeme ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (Lexeme ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LexemeConstraint (SC.Serial m) ann
         ) => SC.Serial m (Lexeme ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LexemeConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (Lexeme ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'Lexeme'.
--
--   @since 0.1.0
type LexemeConstraint (c :: * -> Constraint) (ann :: *)
  = ( LBindConstraint  c ann
    , LFileConstraint  c ann
    , LBuildConstraint c ann
    , LNameConstraint  c ann
    , c [AST.Expr ann]
    , c ann
    )

--------------------------------------------------------------------------------

-- | The name of a Ninja rule or pool.
--
--   @since 0.1.0
data LName ann
  = MkLName
    { _lnameAnn :: !ann
    , _lnameBS  :: !ByteString
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated LName where
  annotation' _ = Lens.lens _lnameAnn
                  $ \(MkLName {..}) x -> MkLName { _lnameAnn = x, .. }

-- | Converts to @{ann: …, name: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (LName ann) where
  toJSON (MkLName {..})
    = [ "ann"  .= _lnameAnn
      , "name" .= Text.decodeUtf8 _lnameBS
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (LName ann) where
  parseJSON = (Aeson.withObject "LName" $ \o -> do
                  _lnameAnn <- (o .: "ann")  >>= pure
                  _lnameBS  <- (o .: "name") >>= Text.encodeUtf8 .> pure
                  pure (MkLName {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (LName ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (LName ann)

-- | Uses the underlying 'SC.Serial' instances.
--
--   @since 0.1.0
instance ( Monad m, LNameConstraint (SC.Serial m) ann
         ) => SC.Serial m (LName ann) where
  series = SC.series |> fmap (second Text.encodeUtf8 .> uncurry MkLName)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LNameConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (LName ann) where
  coseries = SC.coseries .> fmap (\f -> _lnameBS .> Text.decodeUtf8 .> f)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'LName'.
--
--   @since 0.1.0
type LNameConstraint (c :: * -> Constraint) (ann :: *) = (c Text, c ann)

--------------------------------------------------------------------------------

-- | A reference to a file in an @include@ or @subninja@ declaration.
--
--   @since 0.1.0
newtype LFile ann
  = MkLFile
    { _lfileExpr :: AST.Expr ann
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Converts to @{file: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (LFile ann) where
  toJSON (MkLFile {..})
    = [ "file" .= _lfileExpr
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (LFile ann) where
  parseJSON = (Aeson.withObject "LFile" $ \o -> do
                  _lfileExpr <- (o .: "file")  >>= pure
                  pure (MkLFile {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (LFile ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (LFile ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LFileConstraint (SC.Serial m) ann
         ) => SC.Serial m (LFile ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LFileConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (LFile ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'LFile'.
--
--   @since 0.1.0
type LFileConstraint (c :: * -> Constraint) (ann :: *) = (c Text, c ann)

--------------------------------------------------------------------------------

-- | A Ninja variable binding, top-level or otherwise.
--
--   @since 0.1.0
data LBind ann
  = MkLBind
    { _lbindAnn   :: !ann
    , _lbindName  :: !(LName ann)
    , _lbindValue :: !(AST.Expr ann)
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Converts to @{ann: …, name: …, value: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (LBind ann) where
  toJSON (MkLBind {..})
    = [ "ann"   .= _lbindAnn
      , "name"  .= _lbindName
      , "value" .= _lbindValue
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (LBind ann) where
  parseJSON = (Aeson.withObject "LBind" $ \o -> do
                  _lbindAnn   <- (o .: "ann")   >>= pure
                  _lbindName  <- (o .: "name")  >>= pure
                  _lbindValue <- (o .: "value") >>= pure
                  pure (MkLBind {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (LBind ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (LBind ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LBindConstraint (SC.Serial m) ann
         ) => SC.Serial m (LBind ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LBindConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (LBind ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'LBind'.
--
--   @since 0.1.0
type LBindConstraint (c :: * -> Constraint) (ann :: *) = (c Text, c ann)

--------------------------------------------------------------------------------

-- | The data contained within a Ninja @build@ declaration.
--
--   @since 0.1.0
data LBuild ann
  = MkLBuild
    { _lbuildAnn  :: !ann
    , _lbuildOuts :: ![AST.Expr ann]
    , _lbuildRule :: !(LName ann)
    , _lbuildDeps :: ![AST.Expr ann]
    }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Constructor for an 'LBuild'.
--
--   @since 0.1.0
makeLBuild :: forall ann.
              ann
           -- ^ The build annotation
           -> [AST.Expr ann]
           -- ^ The build outputs
           -> LName ann
           -- ^ The rule name
           -> [AST.Expr ann]
           -- ^ The build dependencies
           -> LBuild ann
makeLBuild ann outs rule deps
  = let filterExprs :: [AST.Expr ann] -> [AST.Expr ann]
        filterExprs = filter (\case (AST.Lit   _ "") -> False
                                    (AST.Exprs _ []) -> False
                                    _                -> True)
    in MkLBuild ann (filterExprs outs) rule (filterExprs deps)

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated LBuild where
  annotation' f = Lens.lens _lbuildAnn
                  $ \(MkLBuild {..}) x ->
                      MkLBuild { _lbuildAnn = x
                               , _lbuildOuts = map (fmap f) _lbuildOuts
                               , _lbuildRule = f <$> _lbuildRule
                               , _lbuildDeps = map (fmap f) _lbuildOuts
                               , .. }

-- | Converts to @{ann: …, outs: …, rule: …, deps: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (LBuild ann) where
  toJSON (MkLBuild {..})
    = [ "ann"  .= _lbuildAnn
      , "outs" .= _lbuildOuts
      , "rule" .= _lbuildRule
      , "deps" .= _lbuildDeps
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (LBuild ann) where
  parseJSON = (Aeson.withObject "LBuild" $ \o -> do
                  _lbuildAnn  <- (o .: "ann")  >>= pure
                  _lbuildOuts <- (o .: "outs") >>= pure
                  _lbuildRule <- (o .: "rule") >>= pure
                  _lbuildDeps <- (o .: "deps") >>= pure
                  pure (MkLBuild {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (LBuild ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (LBuild ann)

-- | Uses the underlying 'SC.Serial' instances.
--
--   @since 0.1.0
instance ( Monad m, LBuildConstraint (SC.Serial m) ann
         ) => SC.Serial m (LBuild ann) where
  series = makeLBuild <$> SC.series <~> SC.series <~> SC.series <~> SC.series

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, LBuildConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (LBuild ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for an 'LBuild'.
--
--   @since 0.1.0
type LBuildConstraint (c :: * -> Constraint) (ann :: *) = (c Text, c ann)

--------------------------------------------------------------------------------
