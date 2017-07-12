-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests/ReferenceLexer.hs
--
-- License:
--     Copyright Neil Mitchell 2011-2017.
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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

-- |
--   Module      : Tests.ReferenceLexer
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Lexing is a slow point, the code below is optimised.
module Tests.ReferenceLexer
  ( -- * Lexing
    lexerFile, lexerText, lexerBS

    -- * Types
  , Lexeme (..)
  , LName  (..)
  , LFile  (..)
  , LBind  (..)
  , LBuild (..)
  ) where

import           Control.Arrow             (first)
import           Control.Monad.Error.Class (MonadError (..))

import qualified Data.ByteString.Char8     as BSC8

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text

import           Data.Char                 (isAsciiLower, isAsciiUpper, isDigit)

import           Flow                      ((.>), (|>))

import           Language.Ninja.AST        (Str)

import qualified Language.Ninja.AST        as AST
import qualified Language.Ninja.Errors     as Errors
import qualified Language.Ninja.Misc       as Misc
import qualified Language.Ninja.Mock       as Mock

import           Language.Ninja.Lexer      (LBuild (..), makeLBuild)
import           Language.Ninja.Lexer      (LFile (..))
import           Language.Ninja.Lexer      (LName (..))
import           Language.Ninja.Lexer      (LBind (..))
import           Language.Ninja.Lexer      (Lexeme (..))

import           Tests.ReferenceLexer.Str0 (Str0 (..))
import qualified Tests.ReferenceLexer.Str0 as Str0

--------------------------------------------------------------------------------

-- | Lex the given file.
lexerFile :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
          => Misc.Path -> m [Lexeme ()]
lexerFile file = Mock.readFile file >>= lexerText

-- | Lex the given 'Text'.
lexerText :: (MonadError Errors.ParseError m) => Text -> m [Lexeme ()]
lexerText = Text.encodeUtf8 .> lexerBS

-- | Lex the given 'BSC8.ByteString'.
lexerBS :: (MonadError Errors.ParseError m) => BSC8.ByteString -> m [Lexeme ()]
lexerBS x = lexerLoop (MkStr0 (BSC8.append x "\n\n\0"))

--------------------------------------------------------------------------------

lexerLoop :: (MonadError Errors.ParseError m) => Str0 -> m [Lexeme ()]
lexerLoop c_x
  = case c of
      '\r'                                  -> lexerLoop x0
      '\n'                                  -> lexerLoop x0
      '#'                                   -> lexerLoop $ removeComment x0
      ' '                                   -> lexBind     $ dropSpace x0
      'b'  | Just x1 <- strip "uild "    x0 -> lexBuild    $ dropSpace x1
      'r'  | Just x1 <- strip "ule "     x0 -> lexRule     $ dropSpace x1
      'd'  | Just x1 <- strip "efault "  x0 -> lexDefault  $ dropSpace x1
      'p'  | Just x1 <- strip "ool "     x0 -> lexPool     $ dropSpace x1
      'i'  | Just x1 <- strip "nclude "  x0 -> lexInclude  $ dropSpace x1
      's'  | Just x1 <- strip "ubninja " x0 -> lexSubninja $ dropSpace x1
      '\0'                                  -> pure []
      _                                     -> lexDefine c_x
  where
    removeComment = Str0.dropWhile0 (/= '\n')

    (c, x0) = Str0.list0 c_x

    strip str (MkStr0 x) = let b = BSC8.pack str
                           in if b `BSC8.isPrefixOf` x
                              then Just $ MkStr0 $ BSC8.drop (BSC8.length b) x
                              else Nothing

lexBind :: (MonadError Errors.ParseError m) => Str0 -> m [Lexeme ()]
lexBind c_x | (c, x) <- Str0.list0 c_x
  = case c of
      '\r' -> lexerLoop x
      '\n' -> lexerLoop x
      '#'  -> lexerLoop $ Str0.dropWhile0 (/= '\n') x
      '\0' -> pure []
      _    -> lexxBind (LexBind ()) c_x

lexBuild :: (MonadError Errors.ParseError m) => Str0 -> m [Lexeme ()]
lexBuild x0 = do
  (outputs, x1) <- lexxExprs True x0
  let (rule, x2) = Str0.span0 isVarDot $ dropSpace x1
  (deps, x3) <- lexxExprs False $ dropSpace x2
  x4 <- lexerLoop x3
  pure (LexBuild () (makeLBuild () outputs (MkLName () rule) deps) : x4)

lexDefault :: (MonadError Errors.ParseError m) => Str0 -> m [Lexeme ()]
lexDefault str0 = do
  (files, str1) <- lexxExprs False str0
  str2 <- lexerLoop str1
  pure (LexDefault () files : str2)

lexRule, lexPool, lexInclude, lexSubninja, lexDefine
  :: (MonadError Errors.ParseError m) => Str0 -> m [Lexeme ()]
lexRule     = lexxName (LexRule     ())
lexPool     = lexxName (LexPool     ())
lexInclude  = lexxFile (LexInclude  ())
lexSubninja = lexxFile (LexSubninja ())
lexDefine   = lexxBind (LexDefine   ())

lexxBind :: (MonadError Errors.ParseError m)
         => (LBind () -> Lexeme ()) -> Str0 -> m [Lexeme ()]
lexxBind ctor x0 = do
  let (var,  x1) = Str0.span0 isVarDot x0
  let (eq,   x2) = Str0.list0 $ dropSpace x1
  (expr, x3) <- lexxExpr False False $ dropSpace x2
  x4         <- lexerLoop x3
  if (eq == '=')
    then pure (ctor (MkLBind () (MkLName () var) expr) : x4)
    else Errors.throwLexBindingFailure (Text.pack (show (Str0.take0 100 x0)))

lexxFile :: (MonadError Errors.ParseError m)
         => (LFile () -> Lexeme ()) -> Str0 -> m [Lexeme ()]
lexxFile ctor str0 = do
  (expr, str1) <- lexxExpr False False (dropSpace str0)
  str2         <- lexerLoop str1
  pure (ctor (MkLFile expr) : str2)

lexxName :: (MonadError Errors.ParseError m)
         => (LName () -> Lexeme ()) -> Str0 -> m [Lexeme ()]
lexxName ctor x = do
  let (name, rest) = splitLineCont x
  lexemes <- lexerLoop rest
  pure (ctor (MkLName () name) : lexemes)

lexxExprs :: (MonadError Errors.ParseError m)
          => Bool -> Str0 -> m ([AST.Expr ()], Str0)
lexxExprs sColon x0 = do
  (a, c_x) <- lexxExpr sColon True x0
  let c  = Str0.head0 c_x
  let x1 = Str0.tail0 c_x
  case c of
    ' '           -> do (exprs, x2) <- lexxExprs sColon $ dropSpace x1
                        pure (a:exprs, x2)
    ':'  | sColon -> pure ([a], x1)
    _    | sColon -> Errors.throwLexExpectedColon
    '\r'          -> pure (a $: dropN x1)
    '\n'          -> pure (a $: x1)
    '\0'          -> pure (a $: c_x)
    _             -> Errors.throwLexUnexpectedSeparator c
  where
    ($:) :: AST.Expr () -> Str0 -> ([AST.Expr ()], Str0)
    (AST.Exprs () []) $: s = ([],     s)
    expr              $: s = ([expr], s)

{-# NOINLINE lexxExpr #-}
lexxExpr :: (MonadError Errors.ParseError m)
         => Bool
         -- ^ @stopColon@
         -> Bool
         -- ^ @stopSpace@
         -> Str0
         -- ^ The input bytestring
         -> m (AST.Expr (), Str0)
         -- ^ The second field will start with one of @" :\n\r"@ or be empty
lexxExpr stopColon stopSpace str0 = first exprs <$> f str0
  where
    exprs :: [AST.Expr ()] -> AST.Expr ()
    exprs [x] = AST.normalizeExpr x
    exprs xs  = AST.normalizeExpr (AST.Exprs () xs)

    special :: Char -> Bool
    special x = let b = x `elem` ['$', '\r', '\n', '\0']
                in case (stopColon, stopSpace) of
                     (True , True ) -> (x <= ':') && or [x == ':', x == ' ', b]
                     (True , False) -> (x <= ':') && or [x == ':',           b]
                     (False, True ) -> (x <= '$') && or [          x == ' ', b]
                     (False, False) -> (x <= '$') && or [                    b]

    f :: (MonadError Errors.ParseError m) => Str0 -> m ([AST.Expr ()], Str0)
    f (Str0.break00 special -> (a, x))
      = if BSC8.null a
        then g x
        else g x >>= \y -> pure (AST.Lit () (Text.decodeUtf8 a) $: y)

    g :: (MonadError Errors.ParseError m) => Str0 -> m ([AST.Expr ()], Str0)
    g x0 | (Str0.head0 x0 /= '$') = pure ([], x0)
    g (Str0.tail0 -> c_x) =
      let (c, x0) = Str0.list0 c_x
      in case c of
           '$'   -> f x0 >>= (AST.Lit () "$" $:) .> pure
           ' '   -> f x0 >>= (AST.Lit () " " $:) .> pure
           ':'   -> f x0 >>= (AST.Lit () ":" $:) .> pure
           '\n'  -> f (dropSpace x0)
           '\r'  -> f (dropSpace (dropN x0))
           '{' | (name, x1) <- Str0.span0 isVarDot x0
               , ('}',  x2) <- Str0.list0 x1
               , not (BSC8.null name)
                 -> f x2 >>= (AST.Var () (Text.decodeUtf8 name) $:) .> pure
           _   | (name, x1) <- Str0.span0 isVar c_x
               , not $ BSC8.null name
                 -> f x1 >>= (AST.Var () (Text.decodeUtf8 name) $:) .> pure
           _     -> Errors.throwLexUnexpectedDollar

    ($:) :: a -> ([a], b) -> ([a], b)
    x $: (xs, y) = (x:xs, y)

splitLineCont :: Str0 -> (Str, Str0)
splitLineCont = go .> first BSC8.concat
  where
    go = splitLineCR .> go'

    go' (a, b) | not (endsDollar a) = ([a], b)
               | otherwise          = let (c, d) = go (dropSpace b)
                                      in (BSC8.init a : c, d)

splitLineCR :: Str0 -> (Str, Str0)
splitLineCR x = if BSC8.isSuffixOf (BSC8.singleton '\r') a
                then (BSC8.init a, dropN b)
                else (a, dropN b)
  where
    (a, b) = Str0.break0 (== '\n') x

isVar :: Char -> Bool
isVar x = [ (x == '-')
          , (x == '_')
          , isAsciiLower x
          , isAsciiUpper x
          , isDigit x
          ] |> or

isVarDot :: Char -> Bool
isVarDot x = [ x == '.'
             , isVar x
             ] |> or

endsDollar :: Str -> Bool
endsDollar = BSC8.isSuffixOf (BSC8.singleton '$')

dropN :: Str0 -> Str0
dropN x = if (Str0.head0 x == '\n')
          then Str0.tail0 x
          else x

dropSpace :: Str0 -> Str0
dropSpace = Str0.dropWhile0 (== ' ')

--------------------------------------------------------------------------------
