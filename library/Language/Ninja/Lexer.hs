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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

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
    lexerFile, lexerText, lexerBS

    -- * Types
  , Lexeme (..)
  , LName  (..)
  , LFile  (..)
  , LBind  (..)
  , LBuild (..)
  ) where

import qualified Control.Exception
import           Control.Monad.Error.Class    (MonadError (..))

import qualified Data.ByteString.Char8        as BSC8
import qualified Data.ByteString.Internal     as BS.Internal
import qualified Data.ByteString.Unsafe       as BS.Unsafe

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC8

import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text

import           Data.Char
                 (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Tuple.Extra             (first)

import           Flow                         ((.>), (|>))

import           Data.Aeson                   ((.:), (.=))
import qualified Data.Aeson                   as Aeson

import           Control.DeepSeq              (NFData)
import           Data.Hashable                (Hashable)
import           GHC.Generics                 (Generic)

import           Language.Ninja.AST           (Str)

import qualified Language.Ninja.AST           as AST
import qualified Language.Ninja.Errors        as Err
import qualified Language.Ninja.Misc          as Misc
import qualified Language.Ninja.Mock          as Mock

import           Language.Ninja.Internal.Str0 (Str0 (..))
import qualified Language.Ninja.Internal.Str0 as Str0

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
    { _lnameStr :: Str
    }
  deriving (Eq, Show, Generic)

-- | Converts to a JSON string.
instance Aeson.ToJSON LName where
  toJSON (MkLName {..}) = Aeson.toJSON (Text.decodeUtf8 _lnameStr)

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

-- | FIXME: doc
makeLBuild outs rule deps
  = let filterExprs = filter (\e -> e /= (AST.Exprs []))
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
lexerFile file = Mock.readFile file >>= lexerText

-- | Lex the given 'Text'.
lexerText :: (MonadError Err.ParseError m) => Text -> m [Lexeme]
lexerText = Text.encodeUtf8 .> lexerBS

-- | Lex the given 'BSC8.ByteString'.
lexerBS :: (MonadError Err.ParseError m) => BSC8.ByteString -> m [Lexeme]
lexerBS x = lexerLoop (MkStr0 (BSC8.append x "\n\n\0"))

--------------------------------------------------------------------------------

lexerLoop :: (MonadError Err.ParseError m) => Str0 -> m [Lexeme]
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

lexBind :: (MonadError Err.ParseError m) => Str0 -> m [Lexeme]
lexBind c_x | (c, x) <- Str0.list0 c_x
  = case c of
      '\r' -> lexerLoop x
      '\n' -> lexerLoop x
      '#'  -> lexerLoop $ Str0.dropWhile0 (/= '\n') x
      '\0' -> pure []
      _    -> lexxBind LexBind c_x

lexBuild :: (MonadError Err.ParseError m) => Str0 -> m [Lexeme]
lexBuild x0 = do
  (outputs, x1) <- lexxExprs True x0
  let (rule, x2) = Str0.span0 isVarDot $ dropSpace x1
  (deps, x3) <- lexxExprs False $ dropSpace x2
  x4 <- lexerLoop x3
  pure (LexBuild (makeLBuild outputs (MkLName rule) deps) : x4)

lexDefault :: (MonadError Err.ParseError m) => Str0 -> m [Lexeme]
lexDefault str0 = do
  (files, str1) <- lexxExprs False str0
  str2 <- lexerLoop str1
  pure (LexDefault files : str2)

lexRule, lexPool, lexInclude, lexSubninja, lexDefine
  :: (MonadError Err.ParseError m) => Str0 -> m [Lexeme]
lexRule     = lexxName LexRule
lexPool     = lexxName LexPool
lexInclude  = lexxFile LexInclude
lexSubninja = lexxFile LexSubninja
lexDefine   = lexxBind LexDefine

lexxBind :: (MonadError Err.ParseError m)
         => (LBind -> Lexeme) -> Str0 -> m [Lexeme]
lexxBind ctor x0 = do
  let (var,  x1) = Str0.span0 isVarDot x0
  let (eq,   x2) = Str0.list0 $ dropSpace x1
  (expr, x3) <- lexxExpr False False $ dropSpace x2
  x4         <- lexerLoop x3
  if (eq == '=')
    then pure (ctor (MkLBind (MkLName var) expr) : x4)
    else Err.throwLexBindingFailure (Text.pack (show (Str0.take0 100 x0)))

lexxFile :: (MonadError Err.ParseError m)
         => (LFile -> Lexeme) -> Str0 -> m [Lexeme]
lexxFile ctor str0 = do
  (expr, str1) <- lexxExpr False False (dropSpace str0)
  str2         <- lexerLoop str1
  pure (ctor (MkLFile expr) : str2)

lexxName :: (MonadError Err.ParseError m)
         => (LName -> Lexeme) -> Str0 -> m [Lexeme]
lexxName ctor x = do
  let (name, rest) = splitLineCont x
  lexemes <- lexerLoop rest
  pure (ctor (MkLName name) : lexemes)

lexxExprs :: (MonadError Err.ParseError m)
          => Bool -> Str0 -> m ([AST.Expr], Str0)
lexxExprs sColon x0 = do
  (a, c_x) <- lexxExpr sColon True x0
  let c  = Str0.head0 c_x
  let x1 = Str0.tail0 c_x
  case c of
    ' '           -> do (exprs, x2) <- lexxExprs sColon $ dropSpace x1
                        pure (a:exprs, x2)
    ':'  | sColon -> pure ([a], x1)
    _    | sColon -> Err.throwLexExpectedColon
    '\r'          -> pure (a $: dropN x1)
    '\n'          -> pure (a $: x1)
    '\0'          -> pure (a $: c_x)
    _             -> Err.throwLexUnexpectedSeparator c
  where
    ($:) :: AST.Expr -> Str0 -> ([AST.Expr], Str0)
    (AST.Exprs []) $: s = ([],     s)
    expr           $: s = ([expr], s)

{-# NOINLINE lexxExpr #-}
lexxExpr :: (MonadError Err.ParseError m)
         => Bool
         -- ^ @stopColon@
         -> Bool
         -- ^ @stopSpace@
         -> Str0
         -- ^ The input bytestring
         -> m (AST.Expr, Str0)
         -- ^ The second field will start with one of @" :\n\r"@ or be empty
lexxExpr stopColon stopSpace str0 = first exprs <$> f str0
  where
    exprs :: [AST.Expr] -> AST.Expr
    exprs [x] = AST.normalizeExpr x
    exprs xs  = AST.normalizeExpr (AST.Exprs xs)

    special :: Char -> Bool
    special x = let b = x `elem` ['$', '\r', '\n', '\0']
                in case (stopColon, stopSpace) of
                     (True , True ) -> (x <= ':') && or [x == ':', x == ' ', b]
                     (True , False) -> (x <= ':') && or [x == ':',           b]
                     (False, True ) -> (x <= '$') && or [          x == ' ', b]
                     (False, False) -> (x <= '$') && or [                    b]

    f :: (MonadError Err.ParseError m) => Str0 -> m ([AST.Expr], Str0)
    f (Str0.break00 special -> (a, x))
      = if BSC8.null a
        then g x
        else g x >>= \y -> pure (AST.Lit (Text.decodeUtf8 a) $: y)

    g :: (MonadError Err.ParseError m) => Str0 -> m ([AST.Expr], Str0)
    g x0 | (Str0.head0 x0 /= '$') = pure ([], x0)
    g (Str0.tail0 -> c_x) =
      let (c, x0) = Str0.list0 c_x
      in case c of
           '$'   -> f x0 >>= (AST.Lit "$" $:) .> pure
           ' '   -> f x0 >>= (AST.Lit " " $:) .> pure
           ':'   -> f x0 >>= (AST.Lit ":" $:) .> pure
           '\n'  -> f (dropSpace x0)
           '\r'  -> f (dropSpace (dropN x0))
           '{' | (name, x1) <- Str0.span0 isVarDot x0
               , ('}',  x2) <- Str0.list0 x1
               , not (BSC8.null name)
                 -> f x2 >>= (AST.Var (Text.decodeUtf8 name) $:) .> pure
           _   | (name, x1) <- Str0.span0 isVar c_x
               , not $ BSC8.null name
                 -> f x1 >>= (AST.Var (Text.decodeUtf8 name) $:) .> pure
           _     -> Err.throwLexUnexpectedDollar

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
