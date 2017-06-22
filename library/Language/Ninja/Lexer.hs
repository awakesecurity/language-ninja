-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Lexer.hs
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
--   Module      : Language.Ninja.Lexer
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Lexing is a slow point, the code below is optimised.
module Language.Ninja.Lexer
  ( Lexeme (..)
  , LName (..)
  , LFile (..)
  , LBinding (..)
  , LBuild (..)
  , lexerFile, lexer
  ) where

import qualified Data.ByteString.Char8        as BSC8
import qualified Data.ByteString.Internal     as BS.Internal
import qualified Data.ByteString.Unsafe       as BS.Unsafe

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC8

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T

import           Data.Char                    (isAsciiLower, isAsciiUpper,
                                               isDigit)
import           Data.Tuple.Extra             (first)

import           Flow                         ((|>))

import qualified Data.Aeson                   as Aeson

import           Language.Ninja.Types         (PExpr(..), Str)

import qualified Language.Ninja.Misc.Located  as Loc

import           Language.Ninja.Internal.Str0 (Str0(..))
import qualified Language.Ninja.Internal.Str0

--------------------------------------------------------------------------------

-- | Lex each line separately, rather than each lexeme
data Lexeme
  = -- | @foo = bar@
    LexDefine   !LBinding
  | -- | @[indent]foo = bar@
    LexBind     !LBinding
  | -- | @include file@
    LexInclude  !LFile
  | -- | @subninja file@
    LexSubninja !LFile
  | -- | @build foo: bar | baz || qux@ (@|@ and @||@ are represented as 'PExpr')
    LexBuild    !LBuild
  | -- | @rule name@
    LexRule     !LName
  | -- | @pool name@
    LexPool     !LName
  | -- | @default foo bar@
    LexDefault  ![PExpr]
  deriving (Eq, Show)

-- | FIXME: doc
newtype LName
  = MkLName
    { _lnameStr :: Str
    }
  deriving (Eq, Show)

-- | FIXME: doc
newtype LFile
  = MkLFile
    { _lfileExpr :: PExpr
    }
  deriving (Eq, Show)

-- | FIXME: doc
data LBinding
  = MkLBinding
    { _lbindingName  :: !LName
    , _lbindingValue :: !PExpr
    }
  deriving (Eq, Show)

-- | FIXME: doc
data LBuild
  = MkLBuild
    { _lbuildOuts :: ![PExpr]
    , _lbuildRule :: !Str
    , _lbuildDeps :: ![PExpr]
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | FIXME: doc
lexerFile :: Maybe FilePath -> IO [Lexeme]
lexerFile file = lexer <$> maybe BSC8.getContents BSC8.readFile file

-- | FIXME: doc
lexer :: Str -> [Lexeme]
lexer x = lexerLoop (MkStr0 (BSC8.append x "\n\n\0"))

lexerLoop :: Str0 -> [Lexeme]
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
      '\0'                                -> []
      _                                   -> lexDefine c_x
  where
    removeComment = Language.Ninja.Internal.Str0.dropWhile0 (/= '\n')

    (c, x0) = Language.Ninja.Internal.Str0.list0 c_x

    strip str (MkStr0 x) = let b = BSC8.pack str
                           in if b `BSC8.isPrefixOf` x
                              then Just $ MkStr0 $ BSC8.drop (BSC8.length b) x
                              else Nothing

lexBind :: Str0 -> [Lexeme]
lexBind c_x | (c, x) <- Language.Ninja.Internal.Str0.list0 c_x
  = case c of
      '\r' -> lexerLoop x
      '\n' -> lexerLoop x
      '#'  -> lexerLoop $ Language.Ninja.Internal.Str0.dropWhile0 (/= '\n') x
      '\0' -> []
      _    -> lexxBind LexBind c_x

lexBuild :: Str0 -> [Lexeme]
lexBuild x0
  = let (outputs, x1) = lexxExprs True x0
        (rule,    x2) = Language.Ninja.Internal.Str0.span0 isVarDot $ dropSpace x1
        (deps,    x3) = lexxExprs False $ dropSpace x2
    in LexBuild (MkLBuild outputs rule deps) : lexerLoop x3

lexDefault :: Str0 -> [Lexeme]
lexDefault x = let (files, x') = lexxExprs False x
               in LexDefault files : lexerLoop x'

lexRule, lexPool, lexInclude, lexSubninja, lexDefine :: Str0 -> [Lexeme]
lexRule     = lexxName LexRule
lexPool     = lexxName LexPool
lexInclude  = lexxFile LexInclude
lexSubninja = lexxFile LexSubninja
lexDefine   = lexxBind LexDefine

lexxBind :: (LBinding -> Lexeme) -> Str0 -> [Lexeme]
lexxBind ctor x0 =
  let (var,  x1) = Language.Ninja.Internal.Str0.span0 isVarDot x0
      (eq,   x2) = Language.Ninja.Internal.Str0.list0 $ dropSpace x1
      (expr, x3) = lexxExpr False False $ dropSpace x2
   in if eq == '='
      then ctor (MkLBinding (MkLName var) expr) : lexerLoop x3
      else [ "parse failed when parsing binding: "
           , show (Language.Ninja.Internal.Str0.take0 100 x0)
           ] |> mconcat |> error

lexxFile :: (LFile -> Lexeme) -> Str0 -> [Lexeme]
lexxFile ctor x = let (expr, rest) = lexxExpr False False $ dropSpace x
                  in ctor (MkLFile expr) : lexerLoop rest

lexxName :: (LName -> Lexeme) -> Str0 -> [Lexeme]
lexxName ctor x = let (name, rest) = splitLineCont x
                  in ctor (MkLName name) : lexerLoop rest

lexxExprs :: Bool -> Str0 -> ([PExpr], Str0)
lexxExprs sColon x0
  = let c  = Language.Ninja.Internal.Str0.head0 c_x
        x1 = Language.Ninja.Internal.Str0.tail0 c_x
    in case c of -- FIXME: nonexhaustive pattern match
         ' '           -> first (a:) $ lexxExprs sColon $ dropSpace x1
         ':'  | sColon -> ([a], x1)
         _    | sColon -> error "expected a colon"
         '\r'          -> a $: dropN x1
         '\n'          -> a $: x1
         '\0'          -> a $: c_x
  where
    (a, c_x) = lexxExpr sColon True x0
    ($:) :: PExpr -> Str0 -> ([PExpr], Str0)
    (PExprs []) $: s = ([],     s)
    expr        $: s = ([expr], s)

{-# NOINLINE lexxExpr #-}
lexxExpr :: Bool -> Bool -> Str0
         -> (PExpr, Str0) -- snd will start with one of " :\n\r" or be empty
lexxExpr stopColon stopSpace = first exprs . f
  where
    exprs :: [PExpr] -> PExpr
    exprs [x] = x
    exprs xs  = PExprs xs

    special :: Char -> Bool
    special x = let b = x `elem` ['$', '\r', '\n', '\0']
                in case (stopColon, stopSpace) of
                     (True , True ) -> (x <= ':') && or [x == ':', x == ' ', b]
                     (True , False) -> (x <= ':') && or [x == ':',           b]
                     (False, True ) -> (x <= '$') && or [          x == ' ', b]
                     (False, False) -> (x <= '$') && or [                    b]

    f :: Str0 -> ([PExpr], Str0)
    f (Language.Ninja.Internal.Str0.break00 special -> (a, x))
      = if BSC8.null a then g x else PLit (T.decodeUtf8 a) $: g x

    ($:) :: a -> ([a], b) -> ([a], b)
    x $: (xs, y) = (x:xs, y)

    g :: Str0 -> ([PExpr], Str0)
    g x0 | (Language.Ninja.Internal.Str0.head0 x0 /= '$') =
      ([], x0)
    g (Language.Ninja.Internal.Str0.tail0 -> c_x) =
      let (c, x0) = Language.Ninja.Internal.Str0.list0 c_x
      in case c of
            '$'   -> PLit (T.singleton '$') $: f x0
            ' '   -> PLit (T.singleton ' ') $: f x0
            ':'   -> PLit (T.singleton ':') $: f x0
            '\n'  -> f $ dropSpace x0
            '\r'  -> f $ dropSpace $ dropN x0
            '{' | (name, x1) <- Language.Ninja.Internal.Str0.span0 isVarDot x0
                , ('}',  x2) <- Language.Ninja.Internal.Str0.list0 x1
                , not (BSC8.null name)
                -> PVar (T.decodeUtf8 name) $: f x2
            _   | (name, x1) <- Language.Ninja.Internal.Str0.span0 isVar c_x
                , not $ BSC8.null name
                -> PVar (T.decodeUtf8 name) $: f x1
            _   -> unexpectedDollar

    unexpectedDollar :: a
    unexpectedDollar = error "Unexpect $ followed by unexpected stuff"

splitLineCont :: Str0 -> (Str, Str0)
splitLineCont = first BSC8.concat . f
  where
    f (splitLineCR -> (a, b)) = if not (endsDollar a)
                                then ([a], b)
                                else let (c, d) = f (dropSpace b)
                                     in (BSC8.init a : c, d)

splitLineCR :: Str0 -> (Str, Str0)
splitLineCR x = if BSC8.isSuffixOf (BSC8.singleton '\r') a
                then (BSC8.init a, dropN b)
                else (a, dropN b)
  where
    (a, b) = Language.Ninja.Internal.Str0.break0 (== '\n') x

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
dropN x =
  if (Language.Ninja.Internal.Str0.head0 x == '\n')
  then Language.Ninja.Internal.Str0.tail0 x
  else x

dropSpace :: Str0 -> Str0
dropSpace = Language.Ninja.Internal.Str0.dropWhile0 (== ' ')

--------------------------------------------------------------------------------
