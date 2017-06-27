-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Parse.hs
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

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- FIXME: either rename Parse => Parser or rename Lexer => Lex for consistency

-- |
--   Module      : Language.Ninja.Parse
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Parse a Ninja file.
module Language.Ninja.Parse
  ( parse, parseWithEnv
  ) where

import           Control.Arrow           (second)
import           Control.Monad           ((>=>))

import           Control.Lens.Setter     ((%~), (.~))

import           Data.Monoid             (Endo (..), (<>))

import qualified Data.ByteString.Char8   as BSC8

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HS

import           Language.Ninja.Lexer
                 (LBinding (..), LBuild (..), LFile (..), LName (..),
                 Lexeme (..), lexer)

import qualified Language.Ninja.AST      as AST
import qualified Language.Ninja.AST.Env  as AST
import qualified Language.Ninja.AST.Expr as AST
import qualified Language.Ninja.AST.Rule as AST

import           Flow                    ((.>), (|>))

--------------------------------------------------------------------------------

-- | Parse the file at the given path into a 'PNinja'.
parse :: FilePath -> IO AST.PNinja
parse file = parseWithEnv file AST.makeEnv

-- | Parse the file at the given path using the given Ninja variable context,
--   resulting in a 'PNinja'.
parseWithEnv :: FilePath -> AST.Env Text Text -> IO AST.PNinja
parseWithEnv file env = fst <$> parseFile file (AST.makePNinja, env)

--------------------------------------------------------------------------------

type PNinjaWithEnv = (AST.PNinja, AST.Env Text Text)

parseFile :: FilePath -> PNinjaWithEnv -> IO PNinjaWithEnv
parseFile file (ninja, env) = do
  bs <- if file == "-"
        then BSC8.getContents
        else BSC8.readFile file
  let lexemes = lexer bs
  withBinds lexemes
    |> map (uncurry applyStmt)
    |> foldr (>=>) pure
    |> (\f -> f (ninja, env))
    |> fmap addSpecialVars

addSpecialVars :: PNinjaWithEnv -> PNinjaWithEnv
addSpecialVars (ninja, env) = (mutator ninja, env)
  where
    specialVars :: [Text]
    specialVars = ["ninja_required_version", "builddir"]

    addVariable :: Text -> (AST.PNinja -> AST.PNinja)
    addVariable name = case AST.askEnv env name of
      (Just val) -> AST.pninjaSpecials %~ HM.insert name val
      Nothing    -> id

    mutator :: AST.PNinja -> AST.PNinja
    mutator = map addVariable specialVars |> map Endo |> mconcat |> appEndo

withBinds :: [Lexeme] -> [(Lexeme, [(Text, AST.Expr)])]
withBinds = go
  where
    go []     = []
    go (x:xs) = let (a, b) = f xs
                in (x, a) : withBinds b

    f (LexBind binding : rest) = let MkLBinding (MkLName a) b = binding
                                     (as, bs) = f rest
                                 in ((T.decodeUtf8 a, b) : as, bs)
    f xs                       = ([], xs)

type ApplyFun = [(Text, AST.Expr)] -> PNinjaWithEnv -> IO PNinjaWithEnv

applyStmt :: Lexeme -> ApplyFun
applyStmt lexeme binds (ninja, env)
  = (case lexeme of
       (LexBuild       lbuild) -> applyBuild    lbuild
       (LexRule         lname) -> applyRule     lname
       (LexDefault   defaults) -> applyDefault  defaults
       (LexPool         lname) -> applyPool     lname
       (LexInclude      lfile) -> applyInclude  lfile
       (LexSubninja     lfile) -> applySubninja lfile
       (LexDefine    lbinding) -> applyDefine   lbinding
       (LexBind      lbinding) -> (\_ _ -> throwUnexpectedBinding lbinding))
    |> (\f -> f binds (ninja, env))

applyBuild :: LBuild -> ApplyFun
applyBuild (MkLBuild lexOutputs lexRule lexDeps) lexBinds (ninja, env) = do
  let outputs = map (AST.askExpr env) lexOutputs
  let deps    = HS.fromList (map (AST.askExpr env) lexDeps)
  let binds   = HM.fromList (map (second (AST.askExpr env)) lexBinds)
  let (normal, implicit, orderOnly) = splitDeps deps
  let build = AST.makePBuild (T.decodeUtf8 lexRule) env
              |> (AST.pbuildDeps . AST.pdepsNormal    .~ normal   )
              |> (AST.pbuildDeps . AST.pdepsImplicit  .~ implicit )
              |> (AST.pbuildDeps . AST.pdepsOrderOnly .~ orderOnly)
              |> (AST.pbuildBind                      .~ binds    )
  let allDeps = normal <> implicit <> orderOnly
  let addP = \p -> [(x, allDeps) | x <- outputs] <> (HM.toList p)
                   |> HM.fromList
  let addS = HM.insert (head outputs) build
  let addM = HM.insert (HS.fromList outputs) build
  let ninja' = if lexRule == "phony"
               then ninja |> AST.pninjaPhonys %~ addP
               else if length outputs == 1
               then ninja |> AST.pninjaSingles   %~ addS
               else ninja |> AST.pninjaMultiples %~ addM
  pure (ninja', env)

applyRule :: LName -> ApplyFun
applyRule (MkLName name) binds (ninja, env) = do
  let rule = AST.makeRule |> AST.ruleBind .~ HM.fromList binds
  pure (ninja |> AST.pninjaRules %~ HM.insert (T.decodeUtf8 name) rule, env)

applyDefault :: [AST.Expr] -> ApplyFun
applyDefault lexDefaults _ (ninja, env) = do
  let defaults = HS.fromList (map (AST.askExpr env) lexDefaults)
  pure (ninja |> AST.pninjaDefaults %~ (defaults <>), env)

applyPool :: LName -> ApplyFun
applyPool (MkLName name) binds (ninja, env) = do
  depth <- getDepth env binds
  pure (ninja |> AST.pninjaPools %~ HM.insert (T.decodeUtf8 name) depth, env)

applyInclude :: LFile -> ApplyFun
applyInclude (MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr
  parseFile (T.unpack file) (ninja, env)

applySubninja :: LFile -> ApplyFun
applySubninja (MkLFile expr) _ (ninja, env) = do
  let file = AST.askExpr env expr
  parseFile (T.unpack file) (ninja, AST.scopeEnv env)

applyDefine :: LBinding -> ApplyFun
applyDefine (MkLBinding (MkLName var) value) _ (ninja, env) = do
  pure (ninja, AST.addBind (T.decodeUtf8 var) value env)

-- FIXME: use MonadError instead of IO

throwUnexpectedBinding :: LBinding -> IO a
throwUnexpectedBinding (MkLBinding (MkLName var) _)
  = [ "Unexpected binding defining ", var
    ] |> mconcat |> BSC8.unpack |> fail

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

getDepth :: AST.Env Text Text -> [(Text, AST.Expr)] -> IO Int
getDepth env xs = do
  let poolDepthError x = [ "Could not parse depth field in pool, got: ", x
                         ] |> mconcat |> T.unpack |> error
  case AST.askExpr env <$> lookup "depth" xs of
    Nothing -> pure 1
    Just x  -> case BSC8.readInt (T.encodeUtf8 x) of
                 (Just (i, n)) | BSC8.null n -> pure i
                 _                           -> poolDepthError x

--------------------------------------------------------------------------------
