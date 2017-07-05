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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- |
--   Module      : Language.Ninja.Lexer
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Lexing is a slow point, the code below is optimised.
module Language.Ninja.Lexer
  ( -- * Lexing
    lexerFile
  , lexerText, lexerBS
  , lexerText', lexerBS'
  , lexemesP
  , Parser

    -- * Types
  , Lexeme (..)
  , LName  (..)
  , LFile  (..)
  , LBind  (..)
  , LBuild (..), makeLBuild

    -- * Classes
  , PositionParsing (..)
  ) where

import           Control.Applicative        (Alternative (..))
import qualified Control.Exception
import           Control.Monad              (unless, void, when)
import           Control.Monad.Error.Class  (MonadError (..))

import           Control.Lens               ((^.))
import qualified Control.Lens               as Lens

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC8

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text

import           Data.Char                  (isSpace)
import           Data.Foldable              (asum)
import           Data.Functor               (($>), (<$))
import           Data.Maybe                 (catMaybes, fromMaybe)

import           Flow                       ((.>), (|>))

import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Lexer      as M.Lexer

import           Data.Aeson                 ((.:), (.=))
import qualified Data.Aeson                 as Aeson

import           Control.DeepSeq            (NFData)
import           Data.Hashable              (Hashable)
import           GHC.Generics               (Generic)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Err
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Mock        as Mock

--------------------------------------------------------------------------------

-- | Lex each line separately, rather than each lexeme.
data Lexeme
  = -- | @foo = bar@
    LexDefine   !LBind
  | -- | @[indent]foo = bar@
    LexBind     !LBind
  | -- | @include file@
    LexInclude  !LFile
  | -- | @subninja file@
    LexSubninja !LFile
  | -- | @build foo: bar | baz || qux@
    LexBuild    !LBuild
  | -- | @rule name@
    LexRule     !LName
  | -- | @pool name@
    LexPool     !LName
  | -- | @default foo bar@
    LexDefault  ![AST.Expr]
  deriving (Eq, Show, Generic)

-- | Converts to @{tag: …, value: …}@.
instance Aeson.ToJSON Lexeme where
  toJSON = (\case (LexDefine   value) -> obj "define"   value
                  (LexBind     value) -> obj "bind"     value
                  (LexInclude  value) -> obj "include"  value
                  (LexSubninja value) -> obj "subninja" value
                  (LexBuild    value) -> obj "build"    value
                  (LexRule     value) -> obj "rule"     value
                  (LexPool     value) -> obj "pool"     value
                  (LexDefault  value) -> obj "default"  value)
    where
      obj :: (Aeson.ToJSON a) => Text -> a -> Aeson.Value
      obj tag value = Aeson.object ["tag" .= tag, "value" .= value]

-- | Inverse of the 'ToJSON' instance.
instance Aeson.FromJSON Lexeme where
  parseJSON = (Aeson.withObject "Lexeme" $ \o -> do
                  tag <- o .: "tag"
                  case (tag :: Text) of
                    "define"   -> LexDefine   <$> (o .: "value")
                    "bind"     -> LexBind     <$> (o .: "value")
                    "include"  -> LexInclude  <$> (o .: "value")
                    "subninja" -> LexSubninja <$> (o .: "value")
                    "build"    -> LexBuild    <$> (o .: "value")
                    "rule"     -> LexRule     <$> (o .: "value")
                    "pool"     -> LexPool     <$> (o .: "value")
                    "default"  -> LexDefault  <$> (o .: "value")
                    owise      -> invalidTagError (Text.unpack owise))
    where
      invalidTagError x = [ "Invalid tag: ", x, "; expected one of: "
                          , show [ "default", "bind", "include", "subninja"
                                 , "build", "rule", "pool", "default" ]
                          ] |> mconcat |> fail

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Lexeme

-- | Default 'NFData' instance via 'Generic'.
instance NFData Lexeme

--------------------------------------------------------------------------------

-- | The name of a Ninja rule or pool.
newtype LName
  = MkLName
    { _lnameBS :: ByteString
    }
  deriving (Eq, Show, Generic)

-- | Converts to a JSON string.
instance Aeson.ToJSON LName where
  toJSON (MkLName {..}) = Aeson.toJSON (Text.decodeUtf8 _lnameBS)

-- | Inverse of the 'ToJSON' instance.
instance Aeson.FromJSON LName where
  parseJSON = Aeson.parseJSON .> fmap (Text.encodeUtf8 .> MkLName)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable LName

-- | Default 'NFData' instance via 'Generic'.
instance NFData LName

--------------------------------------------------------------------------------

-- | A reference to a file in an @include@ or @subninja@ declaration.
newtype LFile
  = MkLFile
    { _lfileExpr :: AST.Expr
    }
  deriving (Eq, Show, Generic)

-- | Converts to @{name: …, value: …}@.
instance Aeson.ToJSON LFile where
  toJSON (MkLFile {..})
    = [ "file" .= _lfileExpr
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance Aeson.FromJSON LFile where
  parseJSON = (Aeson.withObject "LFile" $ \o -> do
                  _lfileExpr <- (o .: "file")  >>= pure
                  pure (MkLFile {..}))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable LFile

-- | Default 'NFData' instance via 'Generic'.
instance NFData LFile

--------------------------------------------------------------------------------

-- | A Ninja variable binding, top-level or otherwise.
data LBind
  = MkLBind
    { _lbindName  :: !LName
    , _lbindValue :: !AST.Expr
    }
  deriving (Eq, Show, Generic)

-- | Converts to @{name: …, value: …}@.
instance Aeson.ToJSON LBind where
  toJSON (MkLBind {..})
    = [ "name"  .= _lbindName
      , "value" .= _lbindValue
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance Aeson.FromJSON LBind where
  parseJSON = (Aeson.withObject "LBind" $ \o -> do
                  _lbindName  <- (o .: "name")  >>= pure
                  _lbindValue <- (o .: "value") >>= pure
                  pure (MkLBind {..}))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable LBind

-- | Default 'NFData' instance via 'Generic'.
instance NFData LBind

--------------------------------------------------------------------------------

-- | The data contained within a Ninja @build@ declaration.
data LBuild
  = MkLBuild
    { _lbuildOuts :: ![AST.Expr]
    , _lbuildRule :: !LName
    , _lbuildDeps :: ![AST.Expr]
    }
  deriving (Eq, Show, Generic)

-- | Constructor for an 'LBuild'.
makeLBuild :: [AST.Expr] -> LName -> [AST.Expr] -> LBuild
makeLBuild outs rule deps
  = let filterExprs = filter (\e -> (e /= AST.Lit "") && (e /= AST.Exprs []))
    in MkLBuild (filterExprs outs) rule (filterExprs deps)

-- | Converts to @{outs: …, rule: …, deps: …}@.
instance Aeson.ToJSON LBuild where
  toJSON (MkLBuild {..})
    = [ "outs" .= _lbuildOuts
      , "rule" .= _lbuildRule
      , "deps" .= _lbuildDeps
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance Aeson.FromJSON LBuild where
  parseJSON = (Aeson.withObject "LBuild" $ \o -> do
                  _lbuildOuts <- (o .: "outs") >>= pure
                  _lbuildRule <- (o .: "rule") >>= pure
                  _lbuildDeps <- (o .: "deps") >>= pure
                  pure (MkLBuild {..}))

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable LBuild

-- | Default 'NFData' instance via 'Generic'.
instance NFData LBuild

--------------------------------------------------------------------------------

-- | Lex the given file.
lexerFile :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
          => Misc.Path -> m [Lexeme]
lexerFile file = Mock.readFile file >>= lexerText' (Just file)

-- | Lex the given 'Text'.
lexerText :: (MonadError Err.ParseError m) => Text -> m [Lexeme]
lexerText = lexerText' Nothing

-- | Lex the given 'BSC8.ByteString'.
lexerBS :: (MonadError Err.ParseError m) => ByteString -> m [Lexeme]
lexerBS = lexerBS' Nothing

-- | Lex the given 'Text' that comes from the given 'Misc.Path', if provided.
lexerText' :: (MonadError Err.ParseError m)
           => Maybe Misc.Path -> Text -> m [Lexeme]
lexerText' mp x = let file = fromMaybe "" (Lens.view Misc.pathString <$> mp)
                  in M.runParserT lexemesP file x
                     >>= either Err.throwLexParsecError pure

-- | Lex the given 'ByteString' that comes from the given 'Misc.Path', if it is
--   provided. The 'Misc.Path' is only used for error messages.
lexerBS' :: (MonadError Err.ParseError m)
         => Maybe Misc.Path -> ByteString -> m [Lexeme]
lexerBS' mpath = Text.decodeUtf8 .> lexerText' mpath

--------------------------------------------------------------------------------

-- | This class is kind of like 'DeltaParsing' from @trifecta@.
class (Monad m) => PositionParsing m where
  getPosition :: m Misc.Position

-- | Instance for 'M.ParsecT' from @megaparsec@.
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
type Parser m a = M.ParsecT M.Dec Text m a

-- | The @megaparsec@ parser for a Ninja file.
lexemesP :: (Monad m) => Parser m [Lexeme]
lexemesP = do
  maybes <- [ Nothing <$  lineCommentP
            , Nothing <$  M.separatorChar
            , Nothing <$  M.eol
            , Just    <$> (lexemeP <* lineEndP)
            ] |> asum |> many
  M.eof
  pure (catMaybes maybes)

--------------------------------------------------------------------------------

lexemeP :: (Monad m) => Parser m Lexeme
lexemeP = [ includeP, subninjaP, buildP, ruleP, poolP, defaultP, bindP, defineP
          ] |> map M.try |> asum

defineP :: (Monad m) => Parser m Lexeme
defineP = debugP "defineP"
          (LexDefine <$> equationP)

bindP :: (Monad m) => Parser m Lexeme
bindP = debugP "bindP"
        (LexBind <$> indented f)
  where
    f x | x < 2 = fail "bindP: not indented"
    f _         = equationP

includeP :: (Monad m) => Parser m Lexeme
includeP = debugP "includeP" $ do
  beginningOfLine
  symbolP "include"
  LexInclude <$> M.Lexer.lexeme spaceP fileP

subninjaP :: (Monad m) => Parser m Lexeme
subninjaP = debugP "subninjaP" $ do
  beginningOfLine
  symbolP "subninja"
  LexSubninja <$> M.Lexer.lexeme spaceP fileP

buildP :: (Monad m) => Parser m Lexeme
buildP = debugP "buildP" $ do
  let exprEmpty e = (e == (AST.Lit "")) || (e == (AST.Exprs []))
  let cleanExprs = map AST.normalizeExpr .> filter (exprEmpty .> not)
  beginningOfLine
  symbolP "build"
  outs <- cleanExprs <$> M.some outputP
  symbolP ":"
  rule <- nameP
  deps <- cleanExprs <$> M.many (M.Lexer.lexeme spaceP exprP)
  pure (LexBuild (MkLBuild outs rule deps))

ruleP :: (Monad m) => Parser m Lexeme
ruleP = debugP "ruleP" $ do
  beginningOfLine
  symbolP "rule"
  LexRule <$> nameP

poolP :: (Monad m) => Parser m Lexeme
poolP = debugP "poolP" $ do
  beginningOfLine
  symbolP "pool"
  LexPool <$> nameP

defaultP :: (Monad m) => Parser m Lexeme
defaultP = debugP "defaultP" $ do
  beginningOfLine
  symbolP "default"
  LexDefault <$> M.many (M.Lexer.lexeme spaceP exprP)

lineEndP :: (Monad m) => Parser m ()
lineEndP = do
  M.many M.separatorChar
  lineCommentP <|> pure ()
  void M.eol

equationP :: (Monad m) => Parser m LBind
equationP = debugP "equationP" $ do
  name <- nameP
  symbolP "="
  value <- exprsP
  pure (MkLBind name value)

nameP :: (Monad m) => Parser m LName
nameP = varDotP
        |> fmap (Text.pack .> Text.encodeUtf8 .> MkLName)
        |> M.Lexer.lexeme spaceP
        |> debugP "nameP"

fileP :: (Monad m) => Parser m LFile
fileP = MkLFile <$> exprP
        |> M.Lexer.lexeme spaceP
        |> debugP "fileP"

outputP :: (Monad m) => Parser m AST.Expr
outputP = M.some (dollarP <|> litP)
          |> fmap (AST.Exprs .> AST.normalizeExpr)
          |> M.Lexer.lexeme spaceP
  where
    litP = M.some (M.satisfy isOutputChar)
           |> fmap (Text.pack .> AST.Lit)

    isOutputChar :: Char -> Bool
    isOutputChar '$'             = False
    isOutputChar ':'             = False
    isOutputChar '\n'            = False
    isOutputChar '\r'            = False
    isOutputChar c   | isSpace c = False
    isOutputChar _               = True

exprsP :: (Monad m) => Parser m AST.Expr
exprsP = [ exprP
         , AST.Lit . Text.pack <$> some M.separatorChar
         ] |> asum |> M.many |> fmap (AST.Exprs .> AST.normalizeExpr)

exprP :: (Monad m) => Parser m AST.Expr
exprP = M.some (dollarP <|> litP)
        |> fmap (AST.Exprs .> AST.normalizeExpr)
  where
    litP = M.some (M.satisfy isExprChar) |> fmap (Text.pack .> AST.Lit)

    isExprChar :: Char -> Bool
    isExprChar '$'             = False
    isExprChar '\n'            = False
    isExprChar '\r'            = False
    isExprChar c   | isSpace c = False
    isExprChar _               = True

dollarP :: (Monad m) => Parser m AST.Expr
dollarP = debugP "dollarP"
          (M.char '$'
           *> ([ makeLit <$> M.string "$"
               , makeLit <$> M.string " "
               , makeLit <$> M.string ":"
               , makeLit <$> (M.eol *> M.many M.separatorChar *> pure "")
               , makeVar <$> (M.char '{' *> varDotP <* M.char '}')
               , makeVar <$> varP
               ] |> asum))
  where
    makeLit = Text.pack .> AST.Lit
    makeVar = Text.pack .> AST.Var

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
