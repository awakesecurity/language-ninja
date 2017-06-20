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

-- |
--   Module      : Language.Ninja.Parse
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Parse
  ( parse, parseWithEnv
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad

import           Control.Lens.Getter
import           Control.Lens.Lens
import           Control.Lens.Setter

import           Data.Monoid

import qualified Data.ByteString.Char8 as BSC8

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HS

import           Language.Ninja.Env
import           Language.Ninja.Lexer
import           Language.Ninja.Types

import           Prelude

import           Flow

-- | FIXME: doc
parse :: FilePath -> IO PNinja
parse file = parseWithEnv file makeEnv

-- | FIXME: doc
parseWithEnv :: FilePath -> Env Text Text -> IO PNinja
parseWithEnv file env = fst <$> parseFile file (makePNinja, env)

parseFile :: FilePath
          -> (PNinja, Env Text Text) -> IO (PNinja, Env Text Text)
parseFile file (ninja, env) = do
  lexes <- lexerFile $ if file == "-" then Nothing else Just file
  withBinds lexes
    |> map applyStmt
    |> foldr (>=>) pure
    |> (\f -> f (ninja, env))
    |> fmap addSpecialVars

addSpecialVars :: (PNinja, Env Text Text) -> (PNinja, Env Text Text)
addSpecialVars (ninja, env) = (mutator ninja, env)
  where
    specialVars :: [Text]
    specialVars = ["ninja_required_version", "builddir"]

    addVariable :: Text -> (PNinja -> PNinja)
    addVariable name = case askEnv env name of
                         (Just val) -> pninjaSpecials %~ HM.insert name val
                         Nothing    -> id

    mutator :: PNinja -> PNinja
    mutator = map addVariable specialVars |> map Endo |> mconcat |> appEndo

withBinds :: [Lexeme] -> [(Lexeme, [(Text, PExpr)])]
withBinds = go
  where
    go []     = []
    go (x:xs) = let (a, b) = f xs
                in (x, a) : withBinds b

    f ((LexBind a b) : rest) = let (as, bs) = f rest
                               in ((T.decodeUtf8 a, b) : as, bs)
    f xs                     = ([], xs)

applyStmt :: (Lexeme, [(Text, PExpr)])
          -> (PNinja, Env Text Text) -> IO (PNinja, Env Text Text)
applyStmt (lexKey, lexBinds) (ninja, env) = case lexKey of
  (LexBuild lexOutputs lexRule lexDeps) -> do
    let outputs = map (askExpr env) lexOutputs
    let deps    = HS.fromList (map (askExpr env) lexDeps)
    let binds   = HM.fromList (map (second (askExpr env)) lexBinds)
    let (normal, implicit, orderOnly) = splitDeps deps
    let build = makePBuild (T.decodeUtf8 lexRule) env
                |> (pbuildDeps . pdepsNormal    .~ normal)
                |> (pbuildDeps . pdepsImplicit  .~ implicit)
                |> (pbuildDeps . pdepsOrderOnly .~ orderOnly)
                |> (pbuildBind                  .~ binds)
    let allDeps = normal <> implicit <> orderOnly
    let addP = \p -> [(x, allDeps) | x <- outputs] <> (HM.toList p)
                     |> HM.fromList
    let addS = HM.insert (head outputs) build
    let addM = HM.insert (HS.fromList outputs) build
    let newNinja
          = if      lexRule == "phony"  then ninja |> pninjaPhonys    %~ addP
            else if length outputs == 1 then ninja |> pninjaSingles   %~ addS
            else                             ninja |> pninjaMultiples %~ addM
    pure (newNinja, env)
  (LexRule lexName) -> do
    let rule = makePRule |> pruleBind .~ HM.fromList lexBinds
    pure (ninja |> pninjaRules %~ HM.insert (T.decodeUtf8 lexName) rule, env)
  (LexDefault lexDefaults) -> do
    let defaults = HS.fromList (map (askExpr env) lexDefaults)
    pure (ninja |> pninjaDefaults %~ (defaults <>), env)
  (LexPool lexName) -> do
    depth <- getDepth env lexBinds
    pure (ninja |> pninjaPools %~ HM.insert (T.decodeUtf8 lexName) depth, env)
  (LexInclude lexExpr) -> do
    let file = askExpr env lexExpr
    parseFile (T.unpack file) (ninja, env)
  (LexSubninja lexExpr) -> do
    let file = askExpr env lexExpr
    parseFile (T.unpack file) (ninja, scopeEnv env)
  (LexDefine var value) -> do
    pure (ninja, addBind (T.decodeUtf8 var) value env)
  (LexBind var _) -> [ "Unexpected binding defining ", var
                     ] |> mconcat |> BSC8.unpack |> error

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

getDepth :: Env Text Text -> [(Text, PExpr)] -> IO Int
getDepth env xs = do
  let poolDepthError x = [ "Could not parse depth field in pool, got: ", x
                         ] |> mconcat |> T.unpack |> error
  case askExpr env <$> lookup "depth" xs of
    Nothing -> pure 1
    Just x  -> case BSC8.readInt (T.encodeUtf8 x) of
                 (Just (i, n)) | BSC8.null n -> pure i
                 _                           -> poolDepthError x
