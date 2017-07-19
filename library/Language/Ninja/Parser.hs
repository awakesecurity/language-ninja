-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Parser.hs
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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

-- |
--   Module      : Language.Ninja.Parser
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Parse a Ninja file.
--
--   @since 0.1.0
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

import           Control.Arrow              (second)
import           Control.Exception          (throwIO)
import           Control.Monad              ((>=>))

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Trans.Except (runExceptT)

import qualified Control.Lens               as Lens

import           Data.Maybe                 (Maybe (Just, Nothing))
import           Data.Monoid                (Endo (Endo, appEndo), (<>))

import qualified Data.ByteString.Char8      as BSC8

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text

import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import           Language.Ninja.Lexer       (Ann)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Errors      as Errors
import qualified Language.Ninja.Lexer       as Lexer
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Mock        as Mock

import           Flow                       ((.>), (|>))

--------------------------------------------------------------------------------

-- | Parse the file at the given path into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
parseFileIO :: Misc.Path -> IO (AST.Ninja Ann)
parseFileIO file = forceIO (parseFile file)

-- | Parse the given 'Text' into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
parseTextIO :: Text -> IO (AST.Ninja Ann)
parseTextIO text = forceIO (parseText text)

-- | Parse the given 'BSC8.ByteString' into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
parseBSIO :: BSC8.ByteString -> IO (AST.Ninja Ann)
parseBSIO bs = forceIO (parseBS bs)

-- | Parse the given list of 'Lexer.Lexeme's into a 'AST.Ninja'.
--   This function may throw an exception if parsing fails.
--
--   @since 0.1.0
parseLexemesIO :: [Lexer.Lexeme Ann] -> IO (AST.Ninja Ann)
parseLexemesIO lexemes = forceIO (parseLexemes lexemes)

--------------------------------------------------------------------------------

-- | Parse the file at the given path into a 'AST.Ninja'.
--
--   @since 0.1.0
parseFile :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
          => Misc.Path -> m (AST.Ninja Ann)
parseFile file = parseFileWithEnv file AST.makeEnv

-- | Parse the given 'Text' into a 'AST.Ninja'.
--
--   @since 0.1.0
parseText :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
          => Text -> m (AST.Ninja Ann)
parseText text = parseTextWithEnv text AST.makeEnv

-- | Parse the given 'BSC8.ByteString' into a 'AST.Ninja'.
--
--   @since 0.1.0
parseBS :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
          => BSC8.ByteString -> m (AST.Ninja Ann)
parseBS bs = parseBSWithEnv bs AST.makeEnv

-- | Parse the given list of 'Lexer.Lexeme's into a 'AST.Ninja'.
--
--   @since 0.1.0
parseLexemes :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
             => [Lexer.Lexeme Ann] -> m (AST.Ninja Ann)
parseLexemes lexemes = parseLexemesWithEnv lexemes AST.makeEnv

--------------------------------------------------------------------------------

-- | Parse the file at the given path using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
--
--   @since 0.1.0
parseFileWithEnv :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                 => Misc.Path -> AST.Env Text Text -> m (AST.Ninja Ann)
parseFileWithEnv path env = fst <$> parseFileInternal path (AST.makeNinja, env)

-- | Parse the given 'Text' using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
--
--   @since 0.1.0
parseTextWithEnv :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                 => Text -> AST.Env Text Text -> m (AST.Ninja Ann)
parseTextWithEnv text env = fst <$> parseTextInternal text (AST.makeNinja, env)

-- | Parse the given 'BSC8.ByteString' using the given Ninja variable context,
--   resulting in a 'AST.Ninja'.
--
--   @since 0.1.0
parseBSWithEnv :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                 => BSC8.ByteString -> AST.Env Text Text -> m (AST.Ninja Ann)
parseBSWithEnv bs env = fst <$> parseBSInternal bs (AST.makeNinja, env)

-- | Parse the given list of 'Lexer.Lexeme's into a 'AST.Ninja'.
--
--   @since 0.1.0
parseLexemesWithEnv :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                    => [Lexer.Lexeme Ann] -> AST.Env Text Text
                    -> m (AST.Ninja Ann)
parseLexemesWithEnv lexemes env
  = fst <$> parseLexemesInternal lexemes (AST.makeNinja, env)

--------------------------------------------------------------------------------

type NinjaWithEnv = (AST.Ninja Ann, AST.Env Text Text)

--------------------------------------------------------------------------------

parseFileInternal :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                  => Misc.Path -> NinjaWithEnv -> m NinjaWithEnv
parseFileInternal path (ninja, env) = do
  text <- Mock.readFile path
  parseTextInternal text (ninja, env)

parseTextInternal :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                  => Text -> NinjaWithEnv -> m NinjaWithEnv
parseTextInternal text = parseBSInternal (Text.encodeUtf8 text)

parseBSInternal :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                  => BSC8.ByteString -> NinjaWithEnv -> m NinjaWithEnv
parseBSInternal bs (ninja, env) = do
  lexemes <- Lexer.lexBS bs
  parseLexemesInternal lexemes (ninja, env)

parseLexemesInternal :: (MonadError Errors.ParseError m, Mock.MonadReadFile m)
                     => [Lexer.Lexeme Ann] -> NinjaWithEnv -> m NinjaWithEnv
parseLexemesInternal lexemes (ninja, env) = withBinds lexemes
                                            |> map (uncurry applyStmt)
                                            |> foldr (>=>) pure
                                            |> (\f -> f (ninja, env))
                                            |> fmap addSpecialVars

--------------------------------------------------------------------------------

type MonadApplyFun m = (MonadError Errors.ParseError m, Mock.MonadReadFile m)
type ApplyFun' m = [(Text, AST.Expr Ann)] -> NinjaWithEnv -> m NinjaWithEnv
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

    addVar :: Text -> (AST.Ninja Ann -> AST.Ninja Ann)
    addVar name = case AST.askEnv env name of
                    (Just v) -> Lens.over AST.ninjaSpecials (HM.insert name v)
                    Nothing  -> id

    mutator :: AST.Ninja Ann -> AST.Ninja Ann
    mutator = map addVar specialVars |> map Endo |> mconcat |> appEndo

withBinds :: [Lexer.Lexeme Ann] -> [(Lexer.Lexeme Ann, [(Text, AST.Expr Ann)])]
withBinds = \case []     -> []
                  (x:xs) -> let (a, b) = f xs
                            in (x, a) : withBinds b
  where
    f :: [Lexer.Lexeme Ann] -> ([(Text, AST.Expr Ann)], [Lexer.Lexeme Ann])
    f (Lexer.LexBind _ binding : rest) = let Lexer.MkLBind _ name b = binding
                                             Lexer.MkLName _ a = name
                                             (as, bs) = f rest
                                         in ((Text.decodeUtf8 a, b) : as, bs)
    f xs                               = ([], xs)

--------------------------------------------------------------------------------

applyStmt :: Lexer.Lexeme Ann -> ApplyFun
applyStmt lexeme binds (ninja, env)
  = (case lexeme of
       (Lexer.LexBuild    _ann   lbuild) -> applyBuild    lbuild
       (Lexer.LexRule     _ann    lname) -> applyRule     lname
       (Lexer.LexDefault  _ann defaults) -> applyDefault  defaults
       (Lexer.LexPool     _ann    lname) -> applyPool     lname
       (Lexer.LexInclude  _ann    lfile) -> applyInclude  lfile
       (Lexer.LexSubninja _ann    lfile) -> applySubninja lfile
       (Lexer.LexDefine   _ann    lbind) -> applyDefine   lbind
       (Lexer.LexBind     _ann    lbind) -> throwUnexpectedBinding lbind)
    |> (\f -> f binds (ninja, env))
  where
    -- TODO: don't discard annotation
    throwUnexpectedBinding :: Lexer.LBind Ann -> ApplyFun
    throwUnexpectedBinding (Lexer.MkLBind _ (Lexer.MkLName _ var) _)
      = \_ _ -> Errors.throwParseUnexpectedBinding (Text.decodeUtf8 var)

applyBuild :: Lexer.LBuild Ann -> ApplyFun
applyBuild (Lexer.MkLBuild _annB loutputs lrule ldeps) lbinds (ninja, env) = do
  let outputs = map (AST.askExpr env) loutputs
  let rule    = lrule |> (\(Lexer.MkLName _annN name) -> Text.decodeUtf8 name)
  let deps    = HS.fromList (map (AST.askExpr env) ldeps)
  let binds   = HM.fromList (map (second (AST.askExpr env)) lbinds)
  let (normal, implicit, orderOnly) = splitDeps deps
  let build = AST.makeBuild rule env
              |> (Lens.set (AST.buildDeps . AST.depsNormal)    normal)
              |> (Lens.set (AST.buildDeps . AST.depsImplicit)  implicit)
              |> (Lens.set (AST.buildDeps . AST.depsOrderOnly) orderOnly)
              |> (Lens.set AST.buildBind                       binds)
  let allDeps = normal <> implicit <> orderOnly
  let addP = \p -> [(x, allDeps) | x <- outputs] <> (HM.toList p)
                   |> HM.fromList
  let addS = HM.insert (head outputs) build
  let addM = HM.insert (HS.fromList outputs) build
  let ninja' | (rule == "phony")     = Lens.over AST.ninjaPhonys    addP ninja
             | (length outputs == 1) = Lens.over AST.ninjaSingles   addS ninja
             | otherwise             = Lens.over AST.ninjaMultiples addM ninja
  pure (ninja', env)

applyRule :: Lexer.LName Ann -> ApplyFun
applyRule (Lexer.MkLName ann name) binds (ninja, env) = do
  let rule = AST.makeRule
             |> Lens.set Misc.annotation ann
             |> Lens.set AST.ruleBind    (HM.fromList binds)
  let nameT = Text.decodeUtf8 name
  pure (ninja |> Lens.over AST.ninjaRules (HM.insert nameT rule), env)

applyDefault :: [AST.Expr Ann] -> ApplyFun
applyDefault ldefaults _ (ninja, env) = do
  let defaults = HS.fromList (map (AST.askExpr env) ldefaults)
  pure (ninja |> Lens.over AST.ninjaDefaults (defaults <>), env)

applyPool :: Lexer.LName Ann -> ApplyFun
applyPool (Lexer.MkLName _ann name) binds (ninja, env) = do
  depth <- getDepth env binds
  let nameT = Text.decodeUtf8 name
  pure (ninja |> Lens.over AST.ninjaPools (HM.insert nameT depth), env)

applyInclude :: Lexer.LFile Ann -> ApplyFun
applyInclude (Lexer.MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr |> Lens.view (Lens.from Misc.pathText)
  parseFileInternal file (ninja, env)

applySubninja :: Lexer.LFile Ann -> ApplyFun
applySubninja (Lexer.MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr |> Lens.view (Lens.from Misc.pathText)
  parseFileInternal file (ninja, AST.scopeEnv env)

applyDefine :: Lexer.LBind Ann -> ApplyFun
applyDefine (Lexer.MkLBind _annB (Lexer.MkLName _annN var) value) _ (ninja, env)
  = pure (ninja, AST.addBind (Text.decodeUtf8 var) value env)

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

getDepth :: (MonadError Errors.ParseError m)
         => AST.Env Text Text -> [(Text, AST.Expr Ann)] -> m Int
getDepth env xs
  = case AST.askExpr env <$> lookup "depth" xs of
      Nothing  -> pure 1
      (Just x) -> case BSC8.readInt (Text.encodeUtf8 x) of
                    (Just (i, n)) | n == "" -> pure i
                    _                       -> Errors.throwParseBadDepthField x

--------------------------------------------------------------------------------
