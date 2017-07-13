-- -*- coding: utf-8; mode: haskell; -*-

-- File: executables/NinjaToNix/Pretty.hs
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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- |
--   Module      : NinjaToNix.Pretty
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   A pretty-printer for the types defined in "NinjaToNix.Types".
module NinjaToNix.Pretty
  ( prettySNinja, prettySBuild
  , PP.putDoc
  ) where

import           Data.Maybe                                (fromMaybe)
import           Data.Semigroup                            ((<>))

import           Control.Monad                             (join)

import qualified Control.Lens                              as Lens

import           Data.Text                                 (Text)

import           Data.Graph                                (Graph)
import qualified Data.Graph                                as Graph
import qualified Data.Tree                                 as Tree

import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HM

import           Data.HashSet                              (HashSet)
import qualified Data.HashSet                              as HS

import           Data.Hashable                             (Hashable)

import qualified Data.Text.Prettyprint.Doc                 as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP

import qualified Language.Ninja.IR                         as IR
import qualified Language.Ninja.Misc.Command               as IR

import           Data.List                                 (sort, sortOn)

import           NinjaToNix.Types

import           Flow

import qualified Debug.Trace                               as Trace

--------------------------------------------------------------------------------

type PDoc = PP.Doc PP.AnsiStyle

--------------------------------------------------------------------------------

prettySNinja :: SNinja -> PDoc
prettySNinja = snTopoSort .> go
  where
    go :: ([IR.Target], [(IR.Target, SBuild)]) -> PDoc
    go (defaults, builds) = [ prettyDefaults defaults
                            , PP.vsep (map prettySBuild builds)
                            ] |> PP.vsep

prettyDefaults :: [IR.Target] -> PDoc
prettyDefaults defaults = prettyKeyword "default"
                          <> PP.space
                          <> PP.list (map prettyTarget defaults)
                          <> PP.line

prettySBuild :: (IR.Target, SBuild) -> PDoc
prettySBuild (out, MkSBuild maybeCommand deps) = PP.nest 4
                                                 (prefix <> command <> PP.line)
  where
    prefix = prettyKeyword "build" <> PP.space
             <> prettyOutput out <> PP.colon <> PP.space
             <> PP.list (prettyDependency <$> hsToList deps)

    command = maybe mempty
              (\c -> PP.line <> prettyCommand c)
              maybeCommand

prettyCommand :: IR.Command -> PDoc
prettyCommand = Lens.view IR.commandText
                .> PP.pretty
                .> PP.annotate commandStyle

prettyDependency :: IR.Target -> PDoc
prettyDependency = prettyTarget .> PP.annotate dependencyStyle

prettyOutput :: IR.Target -> PDoc
prettyOutput = prettyTarget .> PP.annotate outputStyle

prettyTarget :: IR.Target -> PDoc
prettyTarget = Lens.view IR.targetText .> PP.pretty .> PP.annotate targetStyle

prettyKeyword :: Text -> PDoc
prettyKeyword = PP.pretty .> PP.annotate keywordStyle

--------------------------------------------------------------------------------

keywordStyle, commandStyle, dependencyStyle, outputStyle, targetStyle
  :: PP.AnsiStyle
keywordStyle    = PP.color PP.Yellow <> PP.bold
commandStyle    = PP.color PP.Red
dependencyStyle = targetStyle
outputStyle     = targetStyle
targetStyle     = PP.color PP.Blue

--------------------------------------------------------------------------------

hsToList :: (Eq a, Ord a, Hashable a) => HashSet a -> [a]
hsToList = HS.toList .> sort

hmToList :: (Eq k, Ord k, Hashable k) => HashMap k v -> [(k, v)]
hmToList = HM.toList .> sortOn fst

snTopoSort :: SNinja -> ([IR.Target], [(IR.Target, SBuild)])
snTopoSort (MkSNinja builds defaults) = (hsToList defaults, ordered)
  where
    ordered :: [(IR.Target, SBuild)]
    ordered = HM.toList targets
              |> sortOn fst
              |> map snd
              |> map (Lens.view IR.targetText)
              |> topologicalSort graph
              |> map IR.makeTarget
              |> mapM resultNode
              |> fromMaybe (error "ordered: something bad happened")

    resultNode :: IR.Target -> Maybe (IR.Target, SBuild)
    resultNode tgt = do
      let maybeBuild = HM.lookup tgt builds
      let cmd  = maybeBuild |> fmap _sbuildCommand |> join
      let deps = maybeBuild |> fmap _sbuildDeps    |> fromMaybe HS.empty
      pure (tgt, MkSBuild cmd deps)

    graph :: Graph
    graph = HM.toList simple
            |> map lookupNode
            |> concatMap explode
            |> Graph.buildG bounds

    explode :: (Int, [Int]) -> [(Int, Int)]
    explode = sequenceA

    bounds :: (Int, Int)
    bounds = let k = HM.keys targets
             in (minimum k, maximum k)

    lookupNode :: (IR.Target, HashSet IR.Target)
               -> (Int, [Int])
    lookupNode (out, deps)
      = let outI = HM.lookup out revTargets
                   |> fromMaybe (error "lookupNode: failed to find out")
            depI = (`HM.lookup` revTargets)
                   .> fromMaybe (error "lookupNode: failed to find dep")
        in (outI, map depI (HS.toList deps))

    revTargets :: HashMap IR.Target Int
    revTargets = HM.toList targets
                 |> map (\(i, t) -> (t, i))
                 |> HM.fromList

    targets :: HashMap Int IR.Target
    targets = HM.toList simple
              |> map (\(out, deps) -> HS.insert out deps)
              |> HS.unions
              |> HS.toList
              |> sort
              |> zip [0..]
              |> HM.fromList

    simple :: HashMap IR.Target (HashSet IR.Target)
    simple = HM.map _sbuildDeps builds

topologicalSort :: forall v. (Eq v, Ord v, Hashable v, Show v)
                => Graph -> [v] -> [v]
topologicalSort graph values = map toTuple values |> sortOn fst |> map snd
  where
    toTuple :: v -> ((Int, Int), v)
    toTuple value = (do tindex <- HM.lookup value sortedTopo
                        oindex <- HM.lookup value sortedByOrd
                        pure ((tindex, oindex), value))
                    |> fromMaybe (error ("toTuple failed on " <> show value))

    sortedByOrd :: HashMap v Int
    sortedByOrd = sort values |> (`zip` [0..]) |> HM.fromList

    sortedTopo :: HashMap v Int
    sortedTopo = Graph.dff graph
                 |> map Tree.levels
                 |> indexForests
                 |> concatMap indexForest
                 |> sortOn fst
                 |> map (\(i, v) -> (values !! v, i))
                 |> HM.fromList

    indexForest :: (Int, [[a]]) -> [(Int, a)]
    indexForest (_, xs) = zip [0..] xs
                          |> concatMap (\(i, level) -> map (i,) level)

    indexForests :: [[[a]]] -> [(Int, [[a]])]
    indexForests = let go :: Int -> [[[a]]] -> [(Int, [[a]])]
                       go _ []     = []
                       go n (x:xs) = (n, x) : go (n + length (concat x)) xs
                   in go 0

--------------------------------------------------------------------------------
