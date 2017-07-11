-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Lexer.hs
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

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
{-# LANGUAGE TupleSections              #-}
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
module Language.Ninja.Lexer
  ( -- * Lexing
    lexerFile
  , lexerText, lexerBS
  , lexerText', lexerBS'
  , lexemesP
  , Parser
  , Ann

    -- * @Lexeme@ and friends
  , Lexeme (..)
  , LName  (..)
  , LFile  (..)
  , LBind  (..)
  , LBuild (..), makeLBuild

    -- * Classes
  , PositionParsing (..)
  ) where

import           Control.Applicative        (Alternative (..))
import           Control.Arrow              (second)
import qualified Control.Exception
import           Control.Monad              (unless, void, when, (>=>))
import           Control.Monad.Error.Class  (MonadError (..))

import           Control.Lens               (lens, (^.))
import qualified Control.Lens               as Lens

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text

import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as NE

import           Data.Char                  (isSpace)
import           Data.Foldable              (asum, maximumBy, minimumBy)
import           Data.Functor               (($>), (<$))
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe)

import           Flow                       ((.>), (|>))

import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Lexer      as M.Lexer

import           Data.Aeson                 ((.:), (.=))
import qualified Data.Aeson                 as Aeson

import           Control.DeepSeq            (NFData)
import           Data.Hashable              (Hashable)
import           Data.Monoid                (Monoid (..))
import           Data.Semigroup             (Semigroup (..))
import           GHC.Generics               (Generic)

import           Test.SmallCheck.Series     ((<~>))
import qualified Test.SmallCheck.Series     as SC

import           GHC.Exts                   (Constraint)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Err
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Mock        as Mock

--------------------------------------------------------------------------------

type Ann = Misc.Spans

--------------------------------------------------------------------------------

spanned :: (Monad m, PositionParsing m) => m a -> m (Ann, a)
spanned p = do
  start  <- getPosition
  result <- p
  end    <- getPosition
  let (sfile, efile) = (start ^. Misc.positionFile, end ^. Misc.positionFile)
  when (sfile /= efile) $ fail "spanned: somehow went over multiple files!"
  let file = sfile
  let offS = start ^. Misc.positionOffset
  let offE = end   ^. Misc.positionOffset
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
  annotation' f = lens (helper .> fst) (helper .> snd)
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
instance (Aeson.ToJSON ann) => Aeson.ToJSON (Lexeme ann) where
  toJSON = (\case (LexDefine   ann value) -> obj ann "define"   value
                  (LexBind     ann value) -> obj ann "bind"     value
                  (LexInclude  ann value) -> obj ann "include"  value
                  (LexSubninja ann value) -> obj ann "subninja" value
                  (LexBuild    ann value) -> obj ann "build"    value
                  (LexRule     ann value) -> obj ann "rule"     value
                  (LexPool     ann value) -> obj ann "pool"     value
                  (LexDefault  ann value) -> obj ann "default"  value)
    where
      obj :: (Aeson.ToJSON ann, Aeson.ToJSON x)
          => ann -> Text -> x -> Aeson.Value
      obj ann tag value = [ "ann" .= ann, "tag" .= tag, "value" .= value
                          ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
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
      invalidTagError x = [ "Invalid tag: ", x, "; expected one of: "
                          , show validTags
                          ] |> mconcat |> fail
      validTags = [ "default", "bind", "include", "subninja"
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
  annotation' f = lens _lnameAnn
                  $ \(MkLName {..}) x -> MkLName { _lnameAnn = x, .. }

-- | Converts to @{ann: …, name: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (LName ann) where
  toJSON (MkLName {..})
    = [ "ann"  .= _lnameAnn
      , "name" .= Text.decodeUtf8 _lnameBS
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
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

-- | Inverse of the 'ToJSON' instance.
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

-- | Inverse of the 'ToJSON' instance.
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
makeLBuild :: ann
           -- ^ The build annotation
           -> [AST.Expr ann]
           -- ^ The build outputs
           -> LName ann
           -- ^ The rule name
           -> [AST.Expr ann]
           -- ^ The build dependencies
           -> LBuild ann
makeLBuild ann outs rule deps
  = let filterExprs = filter (\case (AST.Lit   _ "") -> False
                                    (AST.Exprs _ []) -> False
                                    _                -> True)
    in MkLBuild ann (filterExprs outs) rule (filterExprs deps)

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated LBuild where
  annotation' f = lens _lbuildAnn
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

-- | Inverse of the 'ToJSON' instance.
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

-- | Lex the given file.
--
--   @since 0.1.0
lexerFile :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
          => Misc.Path -> m [Lexeme Ann]
lexerFile file = Mock.readFile file >>= lexerText' (Just file)

-- | Lex the given 'Text'.
--
--   @since 0.1.0
lexerText :: (MonadError Err.ParseError m) => Text -> m [Lexeme Ann]
lexerText = lexerText' Nothing

-- | Lex the given 'BSC8.ByteString'.
--
--   @since 0.1.0
lexerBS :: (MonadError Err.ParseError m) => ByteString -> m [Lexeme Ann]
lexerBS = lexerBS' Nothing

-- | Lex the given 'Text' that comes from the given 'Misc.Path', if provided.
--
--   @since 0.1.0
lexerText' :: (MonadError Err.ParseError m)
           => Maybe Misc.Path -> Text -> m [Lexeme Ann]
lexerText' mp x = let file = fromMaybe "" (Lens.view Misc.pathString <$> mp)
                  in M.runParserT lexemesP file x
                     >>= either Err.throwLexParsecError pure

-- | Lex the given 'ByteString' that comes from the given 'Misc.Path', if it is
--   provided. The 'Misc.Path' is only used for error messages.
--
--   @since 0.1.0
lexerBS' :: (MonadError Err.ParseError m)
         => Maybe Misc.Path -> ByteString -> m [Lexeme Ann]
lexerBS' mpath = Text.decodeUtf8 .> lexerText' mpath

--------------------------------------------------------------------------------

-- | This class is kind of like 'DeltaParsing' from @trifecta@.
--
--   @since 0.1.0
class (Monad m) => PositionParsing m where
  getPosition :: m Misc.Position

-- | Instance for 'M.ParsecT' from @megaparsec@.
--
--   @since 0.1.0
instance (Monad m) => PositionParsing (M.ParsecT M.Dec Text m) where
  getPosition = convert <$> M.getPosition
    where
      convert :: M.SourcePos -> Misc.Position
      convert (M.SourcePos fp line column)
        = let path = fp ^. Lens.from Misc.pathString
          in Misc.makePosition (Just path) (toLine line, toColumn column)

      toLine   :: M.Pos -> Misc.Line
      toColumn :: M.Pos -> Misc.Column
      toLine   = M.unPos .> fromIntegral
      toColumn = M.unPos .> fromIntegral

--------------------------------------------------------------------------------

-- | A @megaparsec@ parser.
--
--   @since 0.1.0
type Parser m a = M.ParsecT M.Dec Text m a

-- | The @megaparsec@ parser for a Ninja file.
--
--   @since 0.1.0
lexemesP :: (Monad m) => Parser m [Lexeme Ann]
lexemesP = do
  maybes <- [ Nothing <$  lineCommentP
            , Nothing <$  M.separatorChar
            , Nothing <$  M.eol
            , Just    <$> (lexemeP <* lineEndP)
            ] |> asum |> many
  M.eof
  pure (catMaybes maybes)

--------------------------------------------------------------------------------

lexemeP :: (Monad m) => Parser m (Lexeme Ann)
lexemeP = [ includeP, subninjaP, buildP, ruleP, poolP, defaultP, bindP, defineP
          ] |> map M.try |> asum

defineP :: (Monad m) => Parser m (Lexeme Ann)
defineP = spanned equationP
          |> fmap (uncurry LexDefine)
          |> debugP "defineP"

bindP :: (Monad m) => Parser m (Lexeme Ann)
bindP = spanned (indented f)
        |> fmap (uncurry LexBind)
        |> debugP "bindP"
  where
    f x | x < 2 = fail "bindP: not indented"
    f _         = equationP

includeP :: (Monad m) => Parser m (Lexeme Ann)
includeP = debugP "includeP" $ do
  (ann, file) <- spanned $ do
    beginningOfLine
    symbolP "include"
    M.Lexer.lexeme spaceP fileP
  pure (LexInclude ann file)

subninjaP :: (Monad m) => Parser m (Lexeme Ann)
subninjaP = debugP "subninjaP" $ do
  (ann, file) <- spanned $ do
    beginningOfLine
    symbolP "subninja"
    M.Lexer.lexeme spaceP fileP
  pure (LexSubninja ann file)

buildP :: (Monad m) => Parser m (Lexeme Ann)
buildP = debugP "buildP" $ do
  let exprEmpty (AST.Lit   _ "") = True
      exprEmpty (AST.Exprs _ []) = True
      exprEmpty _                = False
  let cleanExprs = map AST.normalizeExpr .> filter (exprEmpty .> not)

  (ann, (outs, rule, deps)) <- spanned $ do
    beginningOfLine
    symbolP "build"
    outs <- cleanExprs <$> M.some outputP
    symbolP ":"
    rule <- nameP
    deps <- cleanExprs <$> M.many (M.Lexer.lexeme spaceP exprP)
    pure (outs, rule, deps)

  pure (LexBuild ann (MkLBuild ann outs rule deps))

ruleP :: (Monad m) => Parser m (Lexeme Ann)
ruleP = debugP "ruleP" $ do
  (ann, ruleName) <- spanned $ do
    beginningOfLine
    symbolP "rule"
    nameP
  pure (LexRule ann ruleName)

poolP :: (Monad m) => Parser m (Lexeme Ann)
poolP = debugP "poolP" $ do
  (ann, poolName) <- spanned $ do
    beginningOfLine
    symbolP "pool"
    nameP
  pure (LexPool ann poolName)

defaultP :: (Monad m) => Parser m (Lexeme Ann)
defaultP = debugP "defaultP" $ do
  (ann, defaults) <- spanned $ do
    beginningOfLine
    symbolP "default"
    M.many (M.Lexer.lexeme spaceP exprP)
  pure (LexDefault ann defaults)

lineEndP :: (Monad m) => Parser m ()
lineEndP = do
  M.many M.separatorChar
  lineCommentP <|> pure ()
  void M.eol

equationP :: (Monad m) => Parser m (LBind Ann)
equationP = debugP "equationP" $ do
  (ann, (name, value)) <- spanned $ do
    name <- nameP
    symbolP "="
    value <- exprsP
    pure (name, value)

  pure (MkLBind ann name value)

nameP :: (Monad m) => Parser m (LName Ann)
nameP = spanned varDotP
        |> fmap (second (Text.pack .> Text.encodeUtf8) .> uncurry MkLName)
        |> M.Lexer.lexeme spaceP
        |> debugP "nameP"

fileP :: (Monad m) => Parser m (LFile Ann)
fileP = MkLFile <$> exprP
        |> M.Lexer.lexeme spaceP
        |> debugP "fileP"

outputP :: (Monad m) => Parser m (AST.Expr Ann)
outputP = spanned (M.some (dollarP <|> litP))
          |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
          |> M.Lexer.lexeme spaceP
  where
    litP = spanned (M.some (M.satisfy isOutputChar))
           |> fmap (second Text.pack .> uncurry AST.Lit)

    isOutputChar :: Char -> Bool
    isOutputChar '$'             = False
    isOutputChar ':'             = False
    isOutputChar '\n'            = False
    isOutputChar '\r'            = False
    isOutputChar c   | isSpace c = False
    isOutputChar _               = True

exprsP :: (Monad m) => Parser m (AST.Expr Ann)
exprsP = asum [exprP, separatorP]
         |> M.many |> spanned |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
  where
    separatorP = spanned (some M.separatorChar)
                 |> fmap (second Text.pack .> uncurry AST.Lit)

exprP :: (Monad m) => Parser m (AST.Expr Ann)
exprP = spanned (M.some (dollarP <|> litP))
        |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
  where
    litP = spanned (M.some (M.satisfy isExprChar))
           |> fmap (second Text.pack .> uncurry AST.Lit)

    isExprChar :: Char -> Bool
    isExprChar '$'             = False
    isExprChar '\n'            = False
    isExprChar '\r'            = False
    isExprChar c   | isSpace c = False
    isExprChar _               = True

dollarP :: (Monad m) => Parser m (AST.Expr Ann)
dollarP = debugP "dollarP"
          (M.char '$'
           *> ([ makeLit (M.string "$")
               , makeLit (M.string " ")
               , makeLit (M.string ":")
               , makeLit ((M.eol *> M.many M.separatorChar *> pure ""))
               , makeVar ((M.char '{' *> varDotP <* M.char '}'))
               , makeVar varP
               ] |> asum))
  where
    makeLit p = spanned p |> fmap (second Text.pack .> uncurry AST.Lit)
    makeVar p = spanned p |> fmap (second Text.pack .> uncurry AST.Var)

varDotP :: (Monad m) => Parser m String
varDotP = let char = M.alphaNumChar <|> M.oneOf ['/', '-', '_', '.']
          in debugP "varDotP" (M.some char)

varP :: (Monad m) => Parser m String
varP = let char = M.alphaNumChar <|> M.oneOf ['/', '-', '_']
       in debugP "varP" (M.some char)

symbolP :: (Monad m) => String -> Parser m String
symbolP = M.Lexer.symbol spaceP

spaceP :: (Monad m) => Parser m ()
spaceP = M.Lexer.space (void M.separatorChar) lineCommentP blockCommentP

lineCommentP :: (Monad m) => Parser m ()
lineCommentP = M.Lexer.skipLineComment "#"

blockCommentP :: (Monad m) => Parser m ()
blockCommentP = fail "always"

indented :: (Monad m) => (Misc.Column -> Parser m a) -> Parser m a
indented f = do
  let getCol = Lens.view Misc.positionCol <$> getPosition
  M.many M.separatorChar
  getCol >>= f

beginningOfLine :: (Monad m) => Parser m ()
beginningOfLine = do
  col <- Lens.view Misc.positionCol <$> getPosition
  unless (col == 1) (fail "beginningOfLine failed")

debugP :: (Monad m, Show a) => String -> Parser m a -> Parser m a
debugP = M.label -- M.dbg

--------------------------------------------------------------------------------
