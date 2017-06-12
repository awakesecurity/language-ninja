-- Copyright Neil Mitchell 2011-2017.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Neil Mitchell nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

-- | Lexing is a slow point, the code below is optimised
module Language.Ninja.Lexer
  ( Lexeme (..), lexerFile, lexer
  ) where

import           Control.Applicative
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Unsafe   as BS
import           Data.Char
import           Data.Tuple.Extra
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Exts
import           Prelude
import           System.IO.Unsafe

import           Language.Ninja.Types

---------------------------------------------------------------------
-- LIBRARY BITS

-- A null-terminated strict bytestring.
newtype Str0 = Str0 Str

type S = Ptr Word8

char :: S -> Char
char x = Internal.w2c $ unsafePerformIO $ peek x

next :: S -> S
next x = x `plusPtr` 1

{-# INLINE dropWhile0 #-}
dropWhile0 :: (Char -> Bool) -> Str0 -> Str0
dropWhile0 f x = snd $ span0 f x

{-# INLINE span0 #-}
span0 :: (Char -> Bool) -> Str0 -> (Str, Str0)
span0 f = break0 (not . f)

{-# INLINE break0 #-}
break0 :: (Char -> Bool) -> Str0 -> (Str, Str0)
break0 f (Str0 bs) = (BS.unsafeTake i bs, Str0 $ BS.unsafeDrop i bs)
  where
    i = unsafePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
      let start = castPtr ptr :: S
      let end = go start
      pure $! Ptr end `minusPtr` start

    go s@(Ptr a) | ((c == '\0') || (f c)) = a
                 | otherwise              = go (next s)
      where
        c = char s

{-# INLINE break00 #-}
-- The predicate must return true for '\0'
break00 :: (Char -> Bool) -> Str0 -> (Str, Str0)
break00 f (Str0 bs) = (BS.unsafeTake i bs, Str0 $ BS.unsafeDrop i bs)
  where
    i = unsafePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> do
      let start = castPtr ptr :: S
      let end = go start
      pure $! Ptr end `minusPtr` start

    go s@(Ptr a) | f c       = a
                 | otherwise = go (next s)
      where
        c = char s

head0 :: Str0 -> Char
head0 (Str0 x) = Internal.w2c $ BS.unsafeHead x

tail0 :: Str0 -> Str0
tail0 (Str0 x) = Str0 $ BS.unsafeTail x

list0 :: Str0 -> (Char, Str0)
list0 x = (head0 x, tail0 x)

take0 :: Int -> Str0 -> Str
take0 i (Str0 x) = BS.takeWhile (/= '\0') $ BS.take i x


---------------------------------------------------------------------
-- ACTUAL LEXER

-- | Lex each line separately, rather than each lexeme
data Lexeme
  = LexBind Str Expr
  -- ^ @[indent]foo = bar@
  | LexBuild [Expr] Str [Expr]
  -- ^ @build foo: bar | baz || qux@ (@|@ and @||@ are represented as 'Expr')
  | LexInclude Expr
  -- ^ @include file@
  | LexSubninja Expr
  -- ^ @include file@
  | LexRule Str
  -- ^ @rule name@
  | LexPool Str
  -- ^ @pool name@
  | LexDefault [Expr]
  -- ^ @default foo bar@
  | LexDefine Str Expr
  -- ^ @foo = bar@
  deriving Show

-- | FIXME: doc
lexerFile :: Maybe FilePath -> IO [Lexeme]
lexerFile file = lexer <$> maybe BS.getContents BS.readFile file

-- | FIXME: doc
lexer :: Str -> [Lexeme]
lexer x = lexerLoop (Str0 (BS.append x "\n\n\0"))

lexerLoop :: Str0 -> [Lexeme]
lexerLoop c_x | (c, x) <- list0 c_x
  = case c of
      '\r'                                -> lexerLoop x
      '\n'                                -> lexerLoop x
      '#'                                 -> lexerLoop $ dropWhile0 (/= '\n') x
      ' '                                 -> lexBind     $ dropSpace x
      'b'  | Just x <- strip "uild "    x -> lexBuild    $ dropSpace x
      'r'  | Just x <- strip "ule "     x -> lexRule     $ dropSpace x
      'd'  | Just x <- strip "efault "  x -> lexDefault  $ dropSpace x
      'p'  | Just x <- strip "ool "     x -> lexPool     $ dropSpace x
      'i'  | Just x <- strip "nclude "  x -> lexInclude  $ dropSpace x
      's'  | Just x <- strip "ubninja " x -> lexSubninja $ dropSpace x
      '\0'                                -> []
      _                                   -> lexDefine c_x
  where
    strip str (Str0 x) = let b = BS.pack str
                         in if b `BS.isPrefixOf` x
                            then Just $ Str0 $ BS.drop (BS.length b) x
                            else Nothing

lexBind :: Str0 -> [Lexeme]
lexBind c_x | (c, x) <- list0 c_x
  = case c of
      '\r' -> lexerLoop x
      '\n' -> lexerLoop x
      '#'  -> lexerLoop $ dropWhile0 (/= '\n') x
      '\0' -> []
      _    -> lexxBind LexBind c_x

lexBuild :: Str0 -> [Lexeme]
lexBuild x
    | (outputs, x) <- lexxExprs True x
    , (rule,    x) <- span0 isVarDot $ dropSpace x
    , (deps,    x) <- lexxExprs False $ dropSpace x
    = LexBuild outputs rule deps : lexerLoop x

lexDefault :: Str0 -> [Lexeme]
lexDefault x
    | (files, x) <- lexxExprs False x
    = LexDefault files : lexerLoop x

lexRule, lexPool, lexInclude, lexSubninja, lexDefine :: Str0 -> [Lexeme]
lexRule     = lexxName LexRule
lexPool     = lexxName LexPool
lexInclude  = lexxFile LexInclude
lexSubninja = lexxFile LexSubninja
lexDefine   = lexxBind LexDefine

lexxBind :: (Str -> Expr -> Lexeme) -> Str0 -> [Lexeme]
lexxBind ctor x
  | (var, x) <- span0 isVarDot x
  , ('=', x) <- list0 $ dropSpace x
  , (exp, x) <- lexxExpr False False $ dropSpace x
  = ctor var exp : lexerLoop x
lexxBind _ x
  = error $ show ("parse failed when parsing binding", take0 100 x)

lexxFile :: (Expr -> Lexeme) -> Str0 -> [Lexeme]
lexxFile ctor x
  | (exp, rest) <- lexxExpr False False $ dropSpace x
  = ctor exp : lexerLoop rest

lexxName :: (Str -> Lexeme) -> Str0 -> [Lexeme]
lexxName ctor x
  | (name, rest) <- splitLineCont x
  = ctor name : lexerLoop rest

lexxExprs :: Bool -> Str0 -> ([Expr], Str0)
lexxExprs sColon x
  = case lexxExpr sColon True x of
      (a, c_x)
        | c <- head0 c_x
        , x <- tail0 c_x
          -> case c of -- FIXME: nonexhaustive pattern match
               ' '           -> first (a:) $ lexxExprs sColon $ dropSpace x
               ':'  | sColon -> ([a], x)
               _    | sColon -> error "expected a colon"
               '\r'          -> a $: dropN x
               '\n'          -> a $: x
               '\0'          -> a $: c_x
  where
    Exprs [] $: x = ([], x)
    a $: x = ([a], x)

{-# NOINLINE lexxExpr #-}
lexxExpr :: Bool -> Bool -> Str0 -> (Expr, Str0) -- snd will start with one of " :\n\r" or be empty
lexxExpr stopColon stopSpace = first exprs . f
  where
    exprs [x] = x
    exprs xs  = Exprs xs

    special = case (stopColon, stopSpace) of
                (True , True ) -> \x -> (x <= ':') && (x == ':' || x == ' ' || x == '$' || x == '\r' || x == '\n' || x == '\0')
                (True , False) -> \x -> (x <= ':') && (x == ':'             || x == '$' || x == '\r' || x == '\n' || x == '\0')
                (False, True ) -> \x -> (x <= '$') && (            x == ' ' || x == '$' || x == '\r' || x == '\n' || x == '\0')
                (False, False) -> \x -> (x <= '$') && (                        x == '$' || x == '\r' || x == '\n' || x == '\0')

    f x = case break00 special x of
      (a, x) -> if BS.null a then g x else Lit a $: g x

    x $: (xs,y) = (x:xs,y)

    g x
      | head0 x /= '$'
      = ([], x)
      | c_x <- tail0 x, (c, x) <- list0 c_x
      = case c of
          '$'   -> Lit (BS.singleton '$') $: f x
          ' '   -> Lit (BS.singleton ' ') $: f x
          ':'   -> Lit (BS.singleton ':') $: f x
          '\n'  -> f $ dropSpace x
          '\r'  -> f $ dropSpace $ dropN x
          '{' | (name, x) <- span0 isVarDot x
              , not $ BS.null name
              , ('}', x) <- list0 x
                -> Var name $: f x
          _   | (name, x) <- span0 isVar c_x
              , not $ BS.null name
                -> Var name $: f x
          _     -> error "Unexpect $ followed by unexpected stuff"


splitLineCont :: Str0 -> (Str, Str0)
splitLineCont = first BS.concat . f
  where
    f x = if not (endsDollar a)
          then ([a], b)
          else let (c, d) = f (dropSpace b)
               in (BS.init a : c, d)
      where
        (a, b) = splitLineCR x

splitLineCR :: Str0 -> (Str, Str0)
splitLineCR x = if BS.isSuffixOf (BS.singleton '\r') a
                then (BS.init a, dropN b)
                else (a, dropN b)
  where
    (a, b) = break0 (== '\n') x

isVar :: Char -> Bool
isVar x = (    (x == '-')
            || (x == '_')
            || isAsciiLower x
            || isAsciiUpper x
            || isDigit x
          )

isVarDot :: Char -> Bool
isVarDot x = x == '.' || isVar x

endsDollar :: Str -> Bool
endsDollar = BS.isSuffixOf (BS.singleton '$')

dropN :: Str0 -> Str0
dropN x = if head0 x == '\n' then tail0 x else x

dropSpace :: Str0 -> Str0
dropSpace = dropWhile0 (== ' ')
