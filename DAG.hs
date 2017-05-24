{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DAG
  ( module DAG -- FIXME: specific export list
  ) where

import           Control.Lens

import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Semigroup  (Semigroup)
import qualified Data.Semigroup  as Semigroup

import           Data.Maybe

--------------------------------------------------------------------------------
-- Private API -----------------------------------------------------------------
--------------------------------------------------------------------------------

type Key = Int

data DAG node
  = MkDAG
    { dagNodes :: Set node
    , dagGraph :: Map Key (Set Key)
    }

validKey :: DAG node -> Key -> Bool
validKey dag key = (key >= 0) && (key < Set.size (dagNodes dag))

toKey :: (Ord node) => DAG node -> node -> Maybe Key
toKey dag node = Set.lookupIndex node (dagNodes dag)

fromKey :: (Ord node) => DAG node -> Key -> Maybe node
fromKey dag key
  | validKey dag key = Just (Set.elemAt key (dagNodes dag))
  | otherwise        = Nothing

--------------------------------------------------------------------------------
-- Public API ------------------------------------------------------------------
--------------------------------------------------------------------------------

size :: DAG node -> Int
size = Set.size . dagNodes

getNodes :: DAG node -> Set node
getNodes = dagNodes

getDependents :: (Ord node) => node -> DAG node -> Set node
getDependents node (dag@(MkDAG nodes graph)) = fromMaybe Set.empty $ do
  key <- toKey dag node
  depKeys <- Set.toList <$> Map.lookup key graph
  Set.fromList <$> mapM (fromKey dag) depKeys

contains :: (Ord node) => DAG node -> node -> Bool
contains dag node = Set.member node (dagNodes dag)

addNode :: node -> DAG node -> DAG node
addNode = _

addEdge :: (node, node) -> DAG node -> DAG node
addEdge = _

insertWith :: (Set node -> Set node -> Set node)
           -> (node, Set node) -> DAG node -> DAG node
insertWith combine (node, deps) = _

unionWith :: (Eq node)
          => (node -> node -> node)
          -> DAG node -> DAG node -> DAG node
unionWith = _

union :: (Eq node, Semigroup node) => DAG node -> DAG node -> DAG node
union = _
