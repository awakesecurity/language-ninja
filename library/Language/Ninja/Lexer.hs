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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
  ( -- * @lex*IO@
    lexFileIO
  , lexTextIO
  , lexBSIO

    -- * @lex*@
  , lexFile
  , lexText
  , lexBS

    -- * @lex*WithPath@
  , lexTextWithPath
  , lexBSWithPath

    -- * Other ways of running the lexer
  , lexemesP

    -- * Type aliases
  , Lexer.Parser
  , Lexer.Ann

    -- * @Lexeme@ and friends
  , Lexer.Lexeme (..)
  , Lexer.LName  (..)
  , Lexer.LFile  (..)
  , Lexer.LBind  (..)
  , Lexer.LBuild (..), Lexer.makeLBuild

    -- * Classes
  , Lexer.PositionParsing (..)
  ) where

import           Control.Applicative        (Alternative ((<|>)))
import           Control.Arrow              (second)
import           Control.Exception          (throwIO)
import           Control.Monad              (unless, void, (>=>))
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Trans.Except (runExceptT)

import qualified Control.Lens               as Lens

import           Data.ByteString            (ByteString)

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text

import           Data.Char                  (isSpace)
import           Data.Foldable              (asum)
#if __GLASGOW_HASKELL__ >= 800
import           Data.Functor               ((<$))
#endif
import           Data.Maybe                 (catMaybes, fromMaybe)

import           Flow                       ((.>), (|>))

import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Lexer      as M.Lexer

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Errors
import qualified Language.Ninja.Lexer.Types as Lexer
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Mock        as Mock

--------------------------------------------------------------------------------

-- | Lex the file at the given path.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
lexFileIO :: Misc.Path -> IO [Lexer.Lexeme Lexer.Ann]
lexFileIO = (lexFile .> runExceptT) >=> either throwIO pure

-- | Lex the given 'Text'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
lexTextIO :: Text -> IO [Lexer.Lexeme Lexer.Ann]
lexTextIO = (lexText .> runExceptT) >=> either throwIO pure

-- | Lex the given 'ByteString'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
lexBSIO :: ByteString -> IO [Lexer.Lexeme Lexer.Ann]
lexBSIO = (lexBS .> runExceptT) >=> either throwIO pure

--------------------------------------------------------------------------------

-- | Lex the given file.
--
--   @since 0.1.0
lexFile :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
        => Misc.Path -> m [Lexer.Lexeme Lexer.Ann]
lexFile file = Mock.readFile file >>= lexTextWithPath (Just file)

-- | Lex the given 'Text'.
--
--   @since 0.1.0
lexText :: (MonadError Errors.ParseError m)
        => Text -> m [Lexer.Lexeme Lexer.Ann]
lexText = lexTextWithPath Nothing

-- | Lex the given 'BSC8.ByteString'.
--
--   @since 0.1.0
lexBS :: (MonadError Errors.ParseError m)
      => ByteString -> m [Lexer.Lexeme Lexer.Ann]
lexBS = lexBSWithPath Nothing

--------------------------------------------------------------------------------

-- | Lex the given 'Text' that comes from the given 'Misc.Path', if provided.
--
--   @since 0.1.0
lexTextWithPath :: (MonadError Errors.ParseError m)
                => Maybe Misc.Path -> Text -> m [Lexer.Lexeme Lexer.Ann]
lexTextWithPath mp x = M.runParserT lexemesP file x
                       >>= either Errors.throwLexParsecError pure
  where
    file = fromMaybe "" (Lens.view Misc.pathString <$> mp)

-- | Lex the given 'ByteString' that comes from the given 'Misc.Path', if it is
--   provided. The 'Misc.Path' is only used for error messages.
--
--   @since 0.1.0
lexBSWithPath :: (MonadError Errors.ParseError m)
              => Maybe Misc.Path -> ByteString -> m [Lexer.Lexeme Lexer.Ann]
lexBSWithPath mpath = Text.decodeUtf8 .> lexTextWithPath mpath

--------------------------------------------------------------------------------

-- | The @megaparsec@ parser for a Ninja file.
--
--   @since 0.1.0
lexemesP :: Lexer.Parser m [Lexer.Lexeme Lexer.Ann]
lexemesP = do
  maybes <- [ Nothing <$  lineCommentP
            , Nothing <$  M.separatorChar
            , Nothing <$  M.eol
            , Just    <$> (lexemeP <* lineEndP)
            ] |> asum |> M.many
  M.eof
  pure (catMaybes maybes)

--------------------------------------------------------------------------------

lexemeP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
lexemeP = [ includeP, subninjaP, buildP, ruleP, poolP, defaultP, bindP, defineP
          ] |> map M.try |> asum

defineP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
defineP = Lexer.spanned equationP
          |> fmap (uncurry Lexer.LexDefine)
          |> debugP "defineP"

bindP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
bindP = Lexer.spanned (indented f)
        |> fmap (uncurry Lexer.LexBind)
        |> debugP "bindP"
  where
    f :: Misc.Column -> Lexer.Parser m (Lexer.LBind Lexer.Ann)
    f x | x < 2 = fail "bindP: not indented"
    f _         = equationP

includeP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
includeP = debugP "includeP" $ do
  (ann, file) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "include"
    M.Lexer.lexeme spaceP fileP
  pure (Lexer.LexInclude ann file)

subninjaP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
subninjaP = debugP "subninjaP" $ do
  (ann, file) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "subninja"
    M.Lexer.lexeme spaceP fileP
  pure (Lexer.LexSubninja ann file)

buildP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
buildP = debugP "buildP" $ do
  let isExprEmpty :: AST.Expr Lexer.Ann -> Bool
      isExprEmpty (AST.Lit   _ "") = True
      isExprEmpty (AST.Exprs _ []) = True
      isExprEmpty _                = False

  let cleanExprs :: [AST.Expr Lexer.Ann] -> [AST.Expr Lexer.Ann]
      cleanExprs = map AST.normalizeExpr .> filter (isExprEmpty .> not)

  (ann, (outs, rule, deps)) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "build"
    outs <- cleanExprs <$> M.some outputP
    symbolP ":"
    rule <- nameP
    deps <- cleanExprs <$> M.many (M.Lexer.lexeme spaceP exprP)
    pure (outs, rule, deps)

  pure (Lexer.LexBuild ann (Lexer.MkLBuild ann outs rule deps))

ruleP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
ruleP = debugP "ruleP" $ do
  (ann, ruleName) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "rule"
    nameP
  pure (Lexer.LexRule ann ruleName)

poolP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
poolP = debugP "poolP" $ do
  (ann, poolName) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "pool"
    nameP
  pure (Lexer.LexPool ann poolName)

defaultP :: Lexer.Parser m (Lexer.Lexeme Lexer.Ann)
defaultP = debugP "defaultP" $ do
  (ann, defaults) <- Lexer.spanned $ do
    beginningOfLine
    symbolP "default"
    M.many (M.Lexer.lexeme spaceP exprP)
  pure (Lexer.LexDefault ann defaults)

lineEndP :: Lexer.Parser m ()
lineEndP = do
  M.many M.separatorChar
  lineCommentP <|> pure ()
  void M.eol

equationP :: Lexer.Parser m (Lexer.LBind Lexer.Ann)
equationP = debugP "equationP" $ do
  (ann, (name, value)) <- Lexer.spanned $ do
    name <- nameP
    symbolP "="
    value <- exprsP
    pure (name, value)

  pure (Lexer.MkLBind ann name value)

nameP :: Lexer.Parser m (Lexer.LName Lexer.Ann)
nameP = Lexer.spanned varDotP
        |> fmap (second (Text.pack .> Text.encodeUtf8))
        |> fmap (uncurry Lexer.MkLName)
        |> M.Lexer.lexeme spaceP
        |> debugP "nameP"

fileP :: Lexer.Parser m (Lexer.LFile Lexer.Ann)
fileP = Lexer.MkLFile <$> exprP
        |> M.Lexer.lexeme spaceP
        |> debugP "fileP"

outputP :: Lexer.Parser m (AST.Expr Lexer.Ann)
outputP = Lexer.spanned (M.some (dollarP <|> litP))
          |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
          |> M.Lexer.lexeme spaceP
  where
    litP :: Lexer.Parser m (AST.Expr Lexer.Ann)
    litP = Lexer.spanned (M.some (M.satisfy isOutputChar))
           |> fmap (second Text.pack .> uncurry AST.Lit)

    isOutputChar :: Char -> Bool
    isOutputChar '$'             = False
    isOutputChar ':'             = False
    isOutputChar '\n'            = False
    isOutputChar '\r'            = False
    isOutputChar c   | isSpace c = False
    isOutputChar _               = True

exprsP :: Lexer.Parser m (AST.Expr Lexer.Ann)
exprsP = asum [exprP, separatorP]
         |> M.many
         |> Lexer.spanned
         |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
  where
    separatorP :: Lexer.Parser m (AST.Expr Lexer.Ann)
    separatorP = Lexer.spanned (M.some M.separatorChar)
                 |> fmap (second Text.pack .> uncurry AST.Lit)

exprP :: Lexer.Parser m (AST.Expr Lexer.Ann)
exprP = Lexer.spanned (M.some (dollarP <|> litP))
        |> fmap (uncurry AST.Exprs .> AST.normalizeExpr)
  where
    litP :: Lexer.Parser m (AST.Expr Lexer.Ann)
    litP = Lexer.spanned (M.some (M.satisfy isExprChar))
           |> fmap (second Text.pack .> uncurry AST.Lit)

    isExprChar :: Char -> Bool
    isExprChar '$'             = False
    isExprChar '\n'            = False
    isExprChar '\r'            = False
    isExprChar c   | isSpace c = False
    isExprChar _               = True

dollarP :: Lexer.Parser m (AST.Expr Lexer.Ann)
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
    makeLit :: Lexer.Parser m String -> Lexer.Parser m (AST.Expr Lexer.Ann)
    makeLit p = Lexer.spanned p |> fmap (second Text.pack .> uncurry AST.Lit)

    makeVar :: Lexer.Parser m String -> Lexer.Parser m (AST.Expr Lexer.Ann)
    makeVar p = Lexer.spanned p |> fmap (second Text.pack .> uncurry AST.Var)

varDotP :: Lexer.Parser m String
varDotP = M.some (M.alphaNumChar <|> M.oneOf ['/', '-', '_', '.'])
          |> debugP "varDotP"

varP :: Lexer.Parser m String
varP = M.some (M.alphaNumChar <|> M.oneOf ['/', '-', '_'])
       |> debugP "varP"

symbolP :: String -> Lexer.Parser m String
symbolP = M.Lexer.symbol spaceP

spaceP :: Lexer.Parser m ()
spaceP = M.Lexer.space (void M.separatorChar) lineCommentP blockCommentP

lineCommentP :: Lexer.Parser m ()
lineCommentP = M.Lexer.skipLineComment "#"

blockCommentP :: Lexer.Parser m ()
blockCommentP = fail "always"

indented :: (Misc.Column -> Lexer.Parser m a) -> Lexer.Parser m a
indented f = do
  M.many M.separatorChar
  Lexer.getPosition >>= Lens.view Misc.positionCol .> f

beginningOfLine :: Lexer.Parser m ()
beginningOfLine = do
  col <- Lens.view Misc.positionCol <$> Lexer.getPosition
  unless (col == 1) (fail "beginningOfLine failed")

debugP :: (Show a) => String -> Lexer.Parser m a -> Lexer.Parser m a
debugP str p = M.label str p -- M.dbg str p
  where
    -- this shuts up the compiler wrt the Show constraint
    _ = show <$> p

--------------------------------------------------------------------------------
