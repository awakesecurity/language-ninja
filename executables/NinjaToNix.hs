-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix.hs
--
-- License:
--     Copyright 2017 Awake Security
--
--     Licensed under the Apache License, Version 2.0 (the "License");
--     you may not use this file except in compliance with the License.
--     You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--     Unless required by applicable law or agreed to in writing, software
--     distributed under the License is distributed on an "AS IS" BASIS,
--     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--     See the License for the specific language governing permissions and
--     limitations under the License.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Main
  ( module Main -- FIXME: specific export list
  ) where

import           Control.Lens                as Lens hiding
                 ((.=), (.>), (<.), (<|), (|>))

import           Control.Arrow
import           Control.Monad
import           Data.Either
import           Data.List                   (sort)
import           Data.Maybe
import           Data.Monoid

import           Control.Exception
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.IO                as Text

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC8

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC8

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS

import           Data.Hashable               (Hashable)

import           Language.Ninja.IR.Target    (Target, makeTarget)
import           Language.Ninja.Misc.Command (Command, makeCommand)

import qualified Language.Ninja.AST          as AST
import qualified Language.Ninja.Compile      as Compile
import qualified Language.Ninja.IR           as IR
import qualified Language.Ninja.Misc         as Misc
import qualified Language.Ninja.Parser       as Parser

import           Data.Aeson                  as Aeson
import           Data.Aeson.Encode.Pretty    as Aeson

import           System.Environment          (getArgs)

import           Flow

import           NinjaToNix.Misc.Hash
import           NinjaToNix.Misc.Supply
import           NinjaToNix.Pretty
import           NinjaToNix.Types

--------------------------------------------------------------------------------

parseIO :: (MonadIO m) => FilePath -> m (AST.Ninja ())
parseIO fp = liftIO $ do
  let path = Misc.makePath (Text.pack fp)
  runExceptT (Parser.parseFile path) >>= either throwIO pure

compileToIR :: AST.Ninja () -> IO IR.Ninja
compileToIR ast = either throwIO pure (Compile.compile ast)

simplifyIO :: (MonadIO m) => IR.Ninja -> m SNinja
simplifyIO ninjaIR = liftIO $ do
  result <- runExceptT (simplify ninjaIR)
  case result of
    (Left  e) -> fail (show e)
    (Right x) -> pure x

--------------------------------------------------------------------------------

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (pure ()) f x

simplify :: (MonadError SimplifyError m, MonadIO m) => IR.Ninja -> m SNinja
simplify ninjaIR = runSimplifyT (simplify' ninjaIR) intToTarget
  where
    intToTarget :: Int -> Target
    intToTarget = show .> Text.pack .> ("gen" <>) .> makeTarget

simplify' :: forall m. (Monad m) => IR.Ninja -> SimplifyT m SNinja
simplify' ninjaIR = MkSNinja <$> simpleBuilds <*> simpleDefaults
  where
    -- We first deconstruct all the fields of the input data.

    meta     :: IR.Meta
    builds   :: HashSet IR.Build
    phonys   :: HashMap IR.Target (HashSet IR.Target)
    defaults :: HashSet IR.Target
    pools    :: HashSet IR.Pool
    meta     = ninjaIR ^. IR.ninjaMeta
    builds   = ninjaIR ^. IR.ninjaBuilds
    phonys   = ninjaIR ^. IR.ninjaPhonys
    defaults = ninjaIR ^. IR.ninjaDefaults
    pools    = ninjaIR ^. IR.ninjaPools

    -- Then, we construct the fields of the result.

    simpleBuilds :: SimplifyT m (HashMap Target SBuild)
    simpleBuilds = do
      soPhonys <- HM.unions <$> mapM convertPhony (HM.toList phonys)
      soBuilds <- HM.unions <$> mapM convertBuild (HS.toList builds)
      -- FIXME: use unionWith instead of (<>) in case there is a collision
      optimize (soPhonys <> soBuilds)

    simpleDefaults :: SimplifyT m (HashSet Target)
    simpleDefaults = pure defaults

    -- This is where we will put optimizations in the future. For instance, it
    -- will likely make sense to collapse chains of phonies down to one level.

    optimize :: HashMap Target SBuild
             -> SimplifyT m (HashMap Target SBuild)
    optimize = pure

    -- We don't do anything special in 'convertPhony', for now.

    convertPhony :: (Target, HashSet Target)
                 -> SimplifyT m (HashMap Target SBuild)
    convertPhony (out, deps) = pure (HM.singleton out (MkSBuild Nothing deps))

    -- Currently, 'convertBuild' simply ignores the dependency type, which may
    -- not be the desired semantics in the future, so we should perhaps consider
    -- implementing special semantics for order-only dependencies, if that makes
    -- any sense.

    convertBuild :: IR.Build
                 -> SimplifyT m (HashMap Target SBuild)
    convertBuild b = do
      cmd  <- (b ^. IR.buildRule)
              |> convertRule
      outs <- (b ^. IR.buildOuts)
              |> HS.map (view IR.outputTarget)
              |> pure
      deps <- (b ^. IR.buildDeps)
              |> HS.map (view IR.dependencyTarget)
              |> pure
      let sb = MkSBuild (Just cmd) deps
      case HS.toList outs of
        [out] -> convertSingle   (out,  sb)
        _     -> convertMultiple (outs, sb)

    -- We don't do anything special in 'convertSingle', for now.

    convertSingle :: (Target, SBuild)
                  -> SimplifyT m (HashMap Target SBuild)
    convertSingle (out, build) = pure (HM.singleton out build)

    -- If we have a rule [B] that is part of a build with three outputs and
    -- two dependencies:
    --
    -- @
    --   [FOO] <──┐         ┌──< [INPUT_A]
    --   [BAR] <──┼─< [B] <─┤
    --   [BAZ] <──┘         └──< [INPUT_B]
    -- @
    --
    -- and we run it through 'convertMultiple', then we will get the result of
    -- sending the following set of build edges through 'convertSingle':
    --
    -- @
    --                        ┌──< [INPUT_A]
    --   [gen0001] <──< [B] <─┤
    --                        └──< [INPUT_B]
    --
    --   [FOO] <──< [ ] <──< [gen0001]
    --   [BAR] <──< [ ] <──< [gen0001]
    --   [BAZ] <──< [ ] <──< [gen0001]
    -- @
    --
    -- where @gen1234@ represents a fresh name produced by the concurrent name
    -- supply and @[ ]@ represents a phony build.

    convertMultiple :: (HashSet Target, SBuild)
                    -> SimplifyT m (HashMap Target SBuild)
    convertMultiple (outs, build) = do
      new <- fresh
      let f out = (out, MkSBuild Nothing (HS.singleton new))
      let combined = (new, build) : map f (HS.toList outs)
      mconcat <$> mapM convertSingle combined

    -- Currently this just checks that the rule is not doing anything fancy
    -- that we don't know how to handle yet, and then returns the underlying
    -- command once that is verified.

    convertRule :: IR.Rule -> SimplifyT m Command
    convertRule r = do
      whenJust (r ^. IR.ruleDepfile)      throwUnhandledDepfile
      whenJust (r ^. IR.ruleSpecialDeps)  throwUnhandledSpecialDeps
      when     (r ^. IR.ruleGenerator)    throwUnhandledGenerator
      when     (r ^. IR.ruleRestat)       throwUnhandledRestat
      whenJust (r ^. IR.ruleResponseFile) throwUnhandledResponseFile
      pure (r ^. IR.ruleCommand)

--------------------------------------------------------------------------------

sninjaToJSON :: SNinja -> Value
sninjaToJSON (sn@(MkSNinja {..})) = [ "graph" .= builds, "defaults" .= defaults
                                    ] |> object
  where
    builds, defaults :: Value
    builds   = toJSON (HM.mapWithKey pairJ (HM.map Just _snBuilds <> leafs))
    defaults = toJSON _snDefaults

    leafs :: HashMap Target (Maybe SBuild)
    leafs = leafTargets sn
            |> HS.toList
            |> map (\t -> (t, Nothing))
            |> HM.fromList

    pairJ :: Target -> Maybe SBuild -> Value
    pairJ _      (Just (MkSBuild (Just c) deps)) = buildJ c deps
    pairJ _      (Just (MkSBuild Nothing  deps)) = phonyJ deps
    pairJ target Nothing                         = leafJ target

    buildJ :: Command -> HashSet Target -> Value
    buildJ c deps = tyObject "build" ["dependencies" .= deps, "command" .= c]

    phonyJ :: HashSet Target -> Value
    phonyJ deps = tyObject "phony" ["dependencies" .= deps]

    leafJ :: Target -> Value
    leafJ target = tyObject "leaf" ["path" .= target]

    tyObject t rest = object (("type" .= (t :: Text)) : rest)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [file] <- getArgs
  parsed <- parseIO file
  ir     <- compileToIR parsed
  sninja <- simplifyIO ir
  value  <- pure (sninjaToJSON sninja)
  LBSC8.putStrLn (encodePretty value)

--------------------------------------------------------------------------------
