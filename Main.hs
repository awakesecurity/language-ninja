module Main where

import           Data.Makefile       as Makefile
import qualified Data.Makefile.Parse as Makefile

import           Turtle

import           Control.Arrow
import           Data.Either
import           Data.Maybe

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

import           Data.Set            (Set)
import qualified Data.Set            as Set

-- -- | FIXME: doc
-- data Target
--   = MkTarget
--     { targetName    :: Text
--       -- ^ FIXME: doc
--     , targetCommand :: Text
--       -- ^ FIXME: doc
--     , targetDeps ::
--     }
--
-- -- | FIXME: doc
-- data DepGraph
--   = MkDepGraph
--     { depGraphTargets :: [Target]
--       -- ^ FIXME: doc
--     }

data DAG node
  = MkDAG
    { dagNodeMap :: IntMap node
    , dagGraph   :: _
    }



testMakefile :: IO Makefile
testMakefile = Makefile.parseAsMakefile "./profunctors/src/Makefile"
               >>= either fail pure

type Rule = (Target, [Dependency], [Command])
type Assignment = (ByteString, ByteString)

applyAssignment :: Assignment -> [Entry] -> [Entry]
applyAssignment = _

resolveRules :: Makefile -> [Rule]
resolveRules = go . entries
  where
    go :: [Entry] -> [Rule]
    go ((Assignment lhs rhs):rest) = go (applyAssignment (lhs, rhs) rest)

-- | FIXME: doc
--
--   Law: for any 'Makefile' @mf@, if @m = evalMakefile mf@, then for every key
--   @k ∈ Map.keys m@, @k ∉ snd (Map.lookup m k)@.
--
--   In other words, the dependency graph resulting from this function should
--   never have a cycle, even though it is allowed by the codomain type.
evalMakefile :: Makefile -> Map Target (Set Target, [Command])
evalMakefile = _

main :: IO ()
main = pure ()
