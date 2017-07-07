-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Parser.hs
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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

-- |
--   Module      : Language.Ninja.Parser
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Parse a Ninja file.
module Language.Ninja.Parser
  ( -- * @parse*IO@
    parseFileIO
  , parseTextIO
  , parseBSIO
  , parseLexemesIO

    -- * @parse*@
  , parseFile
  , parseText
  , parseBS
  , parseLexemes

    -- * @parse*WithEnv@
  , parseFileWithEnv
  , parseTextWithEnv
  , parseBSWithEnv
  , parseLexemesWithEnv
  ) where

import           Prelude                    hiding (readFile)
import qualified Prelude

import           Control.Arrow              (second)
import           Control.Exception          (throwIO)
import           Control.Monad              ((>=>))

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Except (runExceptT)

import qualified Control.Lens               as Lens
import           Control.Lens.Getter        (view, (^.))
import           Control.Lens.Setter        ((%~), (.~))

import           Data.Monoid                (Endo (..), (<>))

import qualified Data.ByteString.Char8      as BSC8

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Language.Ninja.Lexer       (Span)
import           Language.Ninja.Lexer
                 (LBind (..), LBuild (..), LFile (..), LName (..), Lexeme (..))
import           Language.Ninja.Misc.Path   (Path, makePath, pathString)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Err
import qualified Language.Ninja.Lexer       as Lexer
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Mock        as Mock

import           Flow                       ((.>), (|>))

--------------------------------------------------------------------------------

-- | Parse the file at the given path into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
parseFileIO :: Path -> IO (AST.Ninja Span)
parseFileIO file = forceIO (parseFile file)

-- | Parse the given 'Text' into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
parseTextIO :: Text -> IO (AST.Ninja Span)
parseTextIO text = forceIO (parseText text)

-- | Parse the given 'BSC8.ByteString' into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
parseBSIO :: BSC8.ByteString -> IO (AST.Ninja Span)
parseBSIO bs = forceIO (parseBS bs)

-- | Parse the given list of 'Lexeme's into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
parseLexemesIO :: [Lexeme Span] -> IO (AST.Ninja Span)
parseLexemesIO lexemes = forceIO (parseLexemes lexemes)

--------------------------------------------------------------------------------

-- | Parse the file at the given path into a 'AST.Ninja'.
parseFile :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
          => Path -> m (AST.Ninja Span)
parseFile file = parseFileWithEnv file AST.makeEnv

-- | Parse the given 'Text' into a 'AST.Ninja'.
parseText :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
          => Text -> m (AST.Ninja Span)
parseText text = parseTextWithEnv text AST.makeEnv

-- | Parse the given 'BSC8.ByteString' into a 'AST.Ninja'.
parseBS :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
          => BSC8.ByteString -> m (AST.Ninja Span)
parseBS bs = parseBSWithEnv bs AST.makeEnv

-- | Parse the given list of 'Lexeme's into a 'AST.Ninja'.
parseLexemes :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
             => [Lexeme Span] -> m (AST.Ninja Span)
parseLexemes lexemes = parseLexemesWithEnv lexemes AST.makeEnv

--------------------------------------------------------------------------------

-- | Parse the file at the given path using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
parseFileWithEnv :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                 => Path -> AST.Env Text Text -> m (AST.Ninja Span)
parseFileWithEnv path env = fst <$> parseFileInternal path (AST.makeNinja, env)

-- | Parse the given 'Text' using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
parseTextWithEnv :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                 => Text -> AST.Env Text Text -> m (AST.Ninja Span)
parseTextWithEnv text env = fst <$> parseTextInternal text (AST.makeNinja, env)

-- | Parse the given 'BSC8.ByteString' using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
parseBSWithEnv :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                 => BSC8.ByteString -> AST.Env Text Text -> m (AST.Ninja Span)
parseBSWithEnv bs env = fst <$> parseBSInternal bs (AST.makeNinja, env)

-- | Parse the given list of 'Lexeme's into a 'AST.Ninja'.
parseLexemesWithEnv :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                    => [Lexeme Span] -> AST.Env Text Text -> m (AST.Ninja Span)
parseLexemesWithEnv lexemes env
  = fst <$> parseLexemesInternal lexemes (AST.makeNinja, env)

--------------------------------------------------------------------------------

type NinjaWithEnv = (AST.Ninja Span, AST.Env Text Text)

--------------------------------------------------------------------------------

parseFileInternal :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                  => Path -> NinjaWithEnv -> m NinjaWithEnv
parseFileInternal path (ninja, env) = do
  text <- Mock.readFile path
  parseTextInternal text (ninja, env)

parseTextInternal :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                  => Text -> NinjaWithEnv -> m NinjaWithEnv
parseTextInternal text = parseBSInternal (Text.encodeUtf8 text)

parseBSInternal :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                  => BSC8.ByteString -> NinjaWithEnv -> m NinjaWithEnv
parseBSInternal bs (ninja, env) = do
  lexemes <- Lexer.lexerBS bs
  parseLexemesInternal lexemes (ninja, env)

parseLexemesInternal :: (MonadError Err.ParseError m, Mock.MonadReadFile m)
                     => [Lexeme Span] -> NinjaWithEnv -> m NinjaWithEnv
parseLexemesInternal lexemes (ninja, env) = withBinds lexemes
                                            |> map (uncurry applyStmt)
                                            |> foldr (>=>) pure
                                            |> (\f -> f (ninja, env))
                                            |> fmap addSpecialVars

--------------------------------------------------------------------------------

type MonadApplyFun m = (MonadError Err.ParseError m, Mock.MonadReadFile m)
type ApplyFun' m = [(Text, AST.Expr Span)] -> NinjaWithEnv -> m NinjaWithEnv
type ApplyFun = (forall m. (MonadApplyFun m) => ApplyFun' m)

--------------------------------------------------------------------------------

forceIO :: (forall m. (MonadApplyFun m) => m a) -> IO a
forceIO action = runExceptT action >>= either throwIO pure

--------------------------------------------------------------------------------

addSpecialVars :: NinjaWithEnv -> NinjaWithEnv
addSpecialVars (ninja, env) = (mutator ninja, env)
  where
    specialVars :: [Text]
    specialVars = ["ninja_required_version", "builddir"]

    addVariable :: Text -> (AST.Ninja Span -> AST.Ninja Span)
    addVariable name = case AST.askEnv env name of
                         (Just val) -> AST.ninjaSpecials %~ HM.insert name val
                         Nothing    -> id

    mutator :: AST.Ninja Span -> AST.Ninja Span
    mutator = map addVariable specialVars |> map Endo |> mconcat |> appEndo

withBinds :: [Lexeme Span] -> [(Lexeme Span, [(Text, AST.Expr Span)])]
withBinds = go
  where
    go []     = []
    go (x:xs) = let (a, b) = f xs
                in (x, a) : withBinds b

    f (LexBind _ binding : rest) = let MkLBind _ (MkLName _ a) b = binding
                                       (as, bs) = f rest
                                   in ((Text.decodeUtf8 a, b) : as, bs)
    f xs                         = ([], xs)

--------------------------------------------------------------------------------

applyStmt :: Lexeme Span -> ApplyFun
applyStmt lexeme binds (ninja, env)
  = (case lexeme of
       (LexBuild    ann   lbuild) -> applyBuild    lbuild
       (LexRule     ann    lname) -> applyRule     lname
       (LexDefault  ann defaults) -> applyDefault  defaults
       (LexPool     ann    lname) -> applyPool     lname
       (LexInclude  ann    lfile) -> applyInclude  lfile
       (LexSubninja ann    lfile) -> applySubninja lfile
       (LexDefine   ann    lbind) -> applyDefine   lbind
       (LexBind     ann    lbind) -> throwUnexpectedBinding lbind)
    |> (\f -> f binds (ninja, env))
  where
    -- FIXME: don't discard annotation
    throwUnexpectedBinding (MkLBind _ (MkLName _ var) _)
      = \_ _ -> Err.throwParseUnexpectedBinding (Text.decodeUtf8 var)

applyBuild :: LBuild Span -> ApplyFun
applyBuild (MkLBuild annB lexOutputs lexRule lexDeps) lexBinds (ninja, env) = do
  let outputs = map (AST.askExpr env) lexOutputs
  let rule    = lexRule |> (\(MkLName annN name) -> Text.decodeUtf8 name)
  let deps    = HS.fromList (map (AST.askExpr env) lexDeps)
  let binds   = HM.fromList (map (second (AST.askExpr env)) lexBinds)
  let (normal, implicit, orderOnly) = splitDeps deps
  let build = AST.makeBuild rule env
              |> (AST.buildDeps . AST.depsNormal    .~ normal   )
              |> (AST.buildDeps . AST.depsImplicit  .~ implicit )
              |> (AST.buildDeps . AST.depsOrderOnly .~ orderOnly)
              |> (AST.buildBind                     .~ binds    )
  let allDeps = normal <> implicit <> orderOnly
  let addP = \p -> [(x, allDeps) | x <- outputs] <> (HM.toList p)
                   |> HM.fromList
  let addS = HM.insert (head outputs) build
  let addM = HM.insert (HS.fromList outputs) build
  let ninja' = if rule ==  "phony"
               then ninja |> AST.ninjaPhonys %~ addP
               else (if length outputs == 1
                     then ninja |> AST.ninjaSingles   %~ addS
                     else ninja |> AST.ninjaMultiples %~ addM)
  pure (ninja', env)

applyRule :: LName Span -> ApplyFun
applyRule (MkLName ann name) binds (ninja, env) = do
  let rule = AST.makeRule
             |> Misc.annotation .~ ann
             |> AST.ruleBind    .~ HM.fromList binds
  pure (ninja |> AST.ninjaRules %~ HM.insert (Text.decodeUtf8 name) rule, env)

applyDefault :: [AST.Expr Span] -> ApplyFun
applyDefault lexDefaults _ (ninja, env) = do
  let defaults = HS.fromList (map (AST.askExpr env) lexDefaults)
  pure (ninja |> AST.ninjaDefaults %~ (defaults <>), env)

applyPool :: LName Span -> ApplyFun
applyPool (MkLName _ name) binds (ninja, env) = do
  -- FIXME: don't discard annotation
  depth <- getDepth env binds
  pure (ninja |> AST.ninjaPools %~ HM.insert (Text.decodeUtf8 name) depth, env)

applyInclude :: LFile Span -> ApplyFun
applyInclude (MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr |> Lens.view (Lens.from Misc.pathText)
  parseFileInternal file (ninja, env)

applySubninja :: LFile Span -> ApplyFun
applySubninja (MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr |> Lens.view (Lens.from Misc.pathText)
  parseFileInternal file (ninja, AST.scopeEnv env)

applyDefine :: LBind Span -> ApplyFun
applyDefine (MkLBind annB (MkLName annN var) value) _ (ninja, env) = do
  -- FIXME: don't discard annotation
  pure (ninja, AST.addBind (Text.decodeUtf8 var) value env)

--------------------------------------------------------------------------------

splitDeps :: HashSet Text -> (HashSet Text, HashSet Text, HashSet Text)
splitDeps = HS.toList
            .> go
            .> (\(a, b, c) -> (HS.fromList a, HS.fromList b, HS.fromList c))
  where
    go :: [Text] -> ([Text], [Text], [Text])
    go []                   = ([],      [],     [])
    go (x:xs) | (x == "|")  = ([],  a <> b,      c)
              | (x == "||") = ([],       b, a <> c)
              | otherwise   = (x:a,      b,      c)
      where
        (a, b, c) = go xs

getDepth :: (MonadError Err.ParseError m)
         => AST.Env Text Text -> [(Text, AST.Expr Span)] -> m Int
getDepth env xs
  = case AST.askExpr env <$> lookup "depth" xs of
      Nothing  -> pure 1
      (Just x) -> case BSC8.readInt (Text.encodeUtf8 x) of
                    (Just (i, n)) | n == "" -> pure i
                    _                       -> Err.throwParseBadDepthField x

--------------------------------------------------------------------------------
