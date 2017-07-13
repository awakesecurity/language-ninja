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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   This executable converts a Ninja file to JSON data that can be given, along
--   with a source path, to a Nix expression that computes a derivation that
--   will execute the Ninja build against the source path in an incremental
--   fashion; i.e.: it generates one Nix derivation per build edge in the
--   Ninja build graph.
--
--   That Nix expression will later be added to this repository, but for now
--   it is available <https://git.io/vQ9mU here>.
--
--   FIXME: move ninja2nix and incremental.nix to a separate repository
module Main (main) where

import qualified Control.Lens                as Lens

import           Control.Monad               (when)
import           Data.Functor                (void)
import           Data.Monoid                 ((<>))

import           Control.Exception           (throwIO)
import           Control.Monad.Error.Class   (MonadError)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Except  (runExceptT)

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Data.ByteString.Lazy.Char8  as LBSC8

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS

import           Language.Ninja.IR.Target    (Target, makeTarget)
import           Language.Ninja.Misc.Command (Command)

import qualified Language.Ninja.AST          as AST
import qualified Language.Ninja.Compile      as Compile
import qualified Language.Ninja.IR           as IR
import qualified Language.Ninja.Misc         as Misc
import qualified Language.Ninja.Parser       as Parser

import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Encode.Pretty    as Aeson

import           System.Environment          (getArgs)

import           Flow

import           NinjaToNix.Misc.Supply
import           NinjaToNix.Types

--------------------------------------------------------------------------------

parseIO :: (MonadIO m) => FilePath -> m (AST.Ninja ())
parseIO fp = liftIO $ do
  let path = Misc.makePath (Text.pack fp)
  runExceptT (Parser.parseFile path)
    >>= either throwIO pure
    >>= void .> pure

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
    meta     = Lens.view IR.ninjaMeta     ninjaIR
    builds   = Lens.view IR.ninjaBuilds   ninjaIR
    phonys   = Lens.view IR.ninjaPhonys   ninjaIR
    defaults = Lens.view IR.ninjaDefaults ninjaIR
    pools    = Lens.view IR.ninjaPools    ninjaIR

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
    convertBuild buildIR = do
      cmd  <- Lens.view IR.buildRule buildIR
              |> convertRule
      outs <- Lens.view IR.buildOuts buildIR
              |> HS.map (Lens.view IR.outputTarget)
              |> pure
      deps <- Lens.view IR.buildDeps buildIR
              |> HS.map (Lens.view IR.dependencyTarget)
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
    convertRule ruleIR = do
      whenJust (Lens.view IR.ruleDepfile      ruleIR) throwUnhandledDepfile
      whenJust (Lens.view IR.ruleSpecialDeps  ruleIR) throwUnhandledSpecialDeps
      when     (Lens.view IR.ruleGenerator    ruleIR) throwUnhandledGenerator
      when     (Lens.view IR.ruleRestat       ruleIR) throwUnhandledRestat
      whenJust (Lens.view IR.ruleResponseFile ruleIR) throwUnhandledResponseFile
      pure (Lens.view IR.ruleCommand ruleIR)

--------------------------------------------------------------------------------

sninjaToJSON :: SNinja -> Aeson.Value
sninjaToJSON (sn@(MkSNinja {..})) = [ "graph" .= graph, "defaults" .= defaults
                                    ] |> Aeson.object
  where
    graph, defaults :: Aeson.Value
    graph    = Aeson.toJSON (HM.mapWithKey pairJ builds)
    defaults = Aeson.toJSON _snDefaults

    builds :: HashMap Target (Maybe SBuild)
    builds = HM.map Just _snBuilds <> leafs

    leafs :: HashMap Target (Maybe SBuild)
    leafs = leafTargets sn
            |> HS.toList
            |> map (\t -> (t, Nothing))
            |> HM.fromList

    pairJ :: Target -> Maybe SBuild -> Aeson.Value
    pairJ _      (Just (MkSBuild (Just c) deps)) = buildJ c deps
    pairJ _      (Just (MkSBuild Nothing  deps)) = phonyJ deps
    pairJ target Nothing                         = leafJ target

    buildJ :: Command -> HashSet Target -> Aeson.Value
    buildJ c deps = tyObject "build" ["dependencies" .= deps, "command" .= c]

    phonyJ :: HashSet Target -> Aeson.Value
    phonyJ deps = tyObject "phony" ["dependencies" .= deps]

    leafJ :: Target -> Aeson.Value
    leafJ target = tyObject "leaf" ["path" .= target]

    tyObject t rest = Aeson.object (("type" .= (t :: Text)) : rest)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [file] <- getArgs
  parsed <- parseIO file
  ir     <- compileToIR parsed
  sninja <- simplifyIO ir
  value  <- pure (sninjaToJSON sninja)
  LBSC8.putStrLn (Aeson.encodePretty value)

--------------------------------------------------------------------------------
