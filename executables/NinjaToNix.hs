-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix.hs
--
-- License:
--     Copyright 2017 Awake Networks
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

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Networks
--   License     : Apache-2.0
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Main where

import           Control.Lens                as Lens hiding
                 ((.=), (.>), (<.), (<|), (|>))

import qualified Data.Makefile               as Makefile
import qualified Data.Makefile.Parse         as Makefile

-- import           Turtle

import           Control.Arrow
import           Data.Either
import           Data.List                   (sort)
import           Data.Maybe
import           Data.Monoid

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC8

import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC8

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS

import           Data.Hashable               (Hashable)

import qualified Language.Ninja.Env          as Ninja
import qualified Language.Ninja.Lexer        as Ninja
import qualified Language.Ninja.Parse        as Ninja
import qualified Language.Ninja.Pretty       as Ninja
import qualified Language.Ninja.Shake        as Ninja
import           Language.Ninja.Types        as Ninja

import           Language.Ninja.AST.Target
import           Language.Ninja.Misc.Command
import           Language.Ninja.Misc.IText   (IText, internText, uninternText)

import           Data.Aeson                  as Aeson
import           Data.Aeson.Encode.Pretty    as Aeson

import           Flow

import           Misc.Hash

--------------------------------------------------------------------------------

pretty :: (ToJSON v) => v -> IO ()
pretty = encodePretty .> LBSC8.putStrLn

debugNinja :: IO Ninja.PNinja
debugNinja = Ninja.parse "../data/build.ninja"

debugSNinja :: IO SNinja
debugSNinja = compileNinja <$> debugNinja

--------------------------------------------------------------------------------

-- | A simplified build graph.
data SNinja
  = MkSNinja
    { _snBuilds   :: !(HashMap Target SBuild)
    , _snDefaults :: !(HashSet Target)
    }
  deriving (Eq, Show)

-- | Look up the unique build rule that outputs the given target.
lookupBuild :: SNinja -> Target -> Maybe SBuild
lookupBuild (MkSNinja builds _) target = HM.lookup target builds

-- | Compute the set of all targets that are an output of a rule.
allOutputs :: SNinja -> HashSet Target
allOutputs (MkSNinja builds _) = HS.fromList (HM.keys builds)

-- | Compute the set of all targets that are a dependency to a rule.
allInputs :: SNinja -> HashSet Target
allInputs (MkSNinja builds _) = mconcat $ map (_sbDeps . snd) $ HM.toList builds

-- | Compute the set of all targets referenced in the build graph.
allTargets :: SNinja -> HashSet Target
allTargets (sn@(MkSNinja _ defs)) = defs <> allOutputs sn <> allInputs sn

-- | Compute the set of all commands that can be run during a build.
allCommands :: SNinja -> HashSet Command
allCommands (MkSNinja builds _) = HS.fromList
                                  $ mapMaybe (_sbCmd . snd)
                                  $ HM.toList builds

-- | Compute the set of targets that have no dependencies.
leafTargets :: SNinja -> HashSet Target
leafTargets (sn@(MkSNinja builds _)) = HS.difference (allTargets sn) outputs
  where
    outputs :: HashSet Target
    outputs = HS.fromList $ HM.keys builds

-- | Compute the set of targets that the given target depends on.
targetReferences :: SNinja -> Target -> HashSet Target
targetReferences (sn@(MkSNinja builds _)) target
  = case lookupBuild sn target of
      Just (MkSBuild _ deps) -> deps
      Nothing                -> HS.empty

-- | Compute the set of targets that depend on the given target.
targetReferrers :: SNinja -> Target -> HashSet Target
targetReferrers = undefined -- FIXME

data SBuild
  = MkSBuild
    { _sbCmd  :: Maybe Command
    , _sbDeps :: HashSet Target
    }
  deriving (Eq, Show)

-- | FIXME: doc
--   Note: this function assumes a very simple subset of Ninja.
compileNinja :: Ninja.PNinja -> SNinja
compileNinja ninja = MkSNinja simpleBuilds simpleDefaults
  where
    onHM :: (Eq k', Hashable k')
         => ((k, v) -> (k', v')) -> HashMap k v -> HashMap k' v'
    onHM f = HM.toList .> map f .> HM.fromList

    simpleBuilds :: HashMap Target SBuild
    simpleBuilds = [ HM.map simplifyBuild combined
                   , onHM simplifyPhony phonys
                   ] |> mconcat |> linearizeGraph

    simpleDefaults :: HashSet Target
    simpleDefaults = HS.map makeTarget defaults

    simplifyBuild :: Ninja.PBuild -> SBuild
    simplifyBuild build = MkSBuild (Just rule) deps
      where
        deps = [ build ^. pbuildDeps . pdepsNormal
               , build ^. pbuildDeps . pdepsImplicit
               , build ^. pbuildDeps . pdepsOrderOnly
               ] |> mconcat |> HS.map makeTarget

        rule = fromMaybe (error ("rule not found: " <> show ruleName))
               $ HM.lookup (makeTarget ruleName) ruleMap

        ruleName = build ^. pbuildRule

    simplifyPhony :: (Text, HashSet Text)
                  -> (HashSet Target, SBuild)
    simplifyPhony (name, files) = ( HS.singleton (makeTarget name)
                                  , MkSBuild Nothing filesT )
      where
        filesT = HS.map makeTarget files

    ruleMap :: HashMap Target Command
    ruleMap = onHM (makeTarget *** computeCommand) rules

    computeCommand :: Ninja.PRule -> Command
    computeCommand rule
      = case HM.lookup "command" (rule ^. pruleBind)
        of Just (Ninja.PLit x) -> makeCommand x
           Just _              -> error "rule uses variables"
           Nothing             -> error "\"command\" not found"

    combined :: HashMap (HashSet Target) Ninja.PBuild
    combined = multiples <> onHM (first HS.singleton) singles
               |> onHM (first (HS.map makeTarget))

    linearizeGraph :: HashMap (HashSet Target) SBuild -> HashMap Target SBuild
    linearizeGraph = HM.toList
                     .> map (first HS.toList)
                     .> concatMap linearizeNode
                     .> HM.fromList

    linearizeNode :: ([Target], SBuild) -> [(Target, SBuild)]
    linearizeNode (outs, build)
      = case uncons (sort outs) of
          Nothing           -> []
          Just (root, rest) -> (root, build) : map (`createEdge` root) rest

    createEdge :: Target -> Target -> (Target, SBuild)
    createEdge x y = (x, MkSBuild Nothing (HS.singleton y))

    rules     = ninja ^. pninjaRules
    singles   = ninja ^. pninjaSingles
    multiples = ninja ^. pninjaMultiples
    phonys    = ninja ^. pninjaPhonys
    defaults  = ninja ^. pninjaDefaults

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

main :: IO ()
main = pure ()
