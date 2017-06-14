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
{-# OPTIONS_HADDOCK show-extensions #-}

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
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Prelude

import           Language.Ninja.Env
import           Language.Ninja.Lexer
import           Language.Ninja.Types

import           Flow

parse :: FilePath -> IO PNinja
parse file = newEnv >>= parseWithEnv file

parseWithEnv :: FilePath -> Env Str Str -> IO PNinja
parseWithEnv file env = parseFile file env newPNinja

parseFile :: FilePath -> Env Str Str -> PNinja -> IO PNinja
parseFile file env ninja = do
  lexes <- lexerFile $ if file == "-" then Nothing else Just file
  foldM (applyStmt env) ninja $ withBinds lexes

withBinds :: [Lexeme] -> [(Lexeme, [(Str, PExpr)])]
withBinds [] = []
withBinds (x:xs) = (x, a) : withBinds b
  where
    (a, b) = f xs
    f ((LexBind a b) : rest) = let (as, bs) = f rest in (((a, b):as), bs)
    f xs                     = ([], xs)

applyStmt :: Env Str Str -> PNinja -> (Lexeme, [(Str, PExpr)]) -> IO PNinja
applyStmt env (ninja@(MkPNinja {..})) (key, binds) = case key of
  (LexBuild outputs rule deps) -> do
    outputs <- mapM (askExpr env) outputs
    deps <- mapM (askExpr env) deps
    binds <- mapM (\(a, b) -> (a,) <$> askExpr env b) binds
    let (normal, implicit, orderOnly) = splitDeps deps
    let build = MkPBuild rule env normal implicit orderOnly binds
    pure $ if      rule == "phony"     then ninja { phonys = [(x, normal <> implicit <> orderOnly) | x <- outputs] <> phonys }
           else if length outputs == 1 then ninja { singles = (head outputs, build) : singles }
           else                             ninja { multiples = (outputs, build) : multiples }
  (LexRule name) -> do
    pure ninja { rules = (name, MkPRule binds) : rules }
  (LexDefault xs) -> do
    xs <- mapM (askExpr env) xs
    pure (ninja { defaults = xs ++ defaults })
  (LexPool name) -> do
    depth <- getDepth env binds
    pure (ninja { pools = (name, depth) : pools })
  (LexInclude expr) -> do
    file <- askExpr env expr
    parseFile (BS.unpack file) env ninja
  (LexSubninja expr) -> do
    file <- askExpr env expr
    e <- scopeEnv env
    parseFile (BS.unpack file) e ninja
  (LexDefine a b) -> do
    addBind env a b
    pure ninja
  (LexBind a _) -> [ "Unexpected binding defining ", a
                   ] |> mconcat |> BS.unpack |> error

splitDeps :: [Str] -> ([Str], [Str], [Str])
splitDeps []                   = ([],      [],     [])
splitDeps (x:xs) | (x == "|")  = ([],  a <> b,      c)
                 | (x == "||") = ([],       b, a <> c)
                 | otherwise   = (x:a,      b,      c)
  where
    (a, b, c) = splitDeps xs

getDepth :: Env Str Str -> [(Str, PExpr)] -> IO Int
getDepth env xs = do
  let poolDepthError x = [ "Could not parse depth field in pool, got: ", x
                         ] |> mconcat |> BS.unpack |> error
  case lookup "depth" xs of
    Nothing -> pure 1
    Just x -> do
      x <- askExpr env x
      case BS.readInt x of
        (Just (i, n)) | BS.null n -> pure i
        _                         -> poolDepthError x
