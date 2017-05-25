{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens        as Lens

import           Data.Makefile       as Makefile
import qualified Data.Makefile.Parse as Makefile

import           Turtle

import           Control.Arrow
import           Data.Either
import           Data.Maybe

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.Set            (Set)
import qualified Data.Set            as Set

newtype Module
  = MkModule Text
  deriving (Eq, Ord, Show)

newtype ModuleGraph
  = MkModuleGraph
    { _moduleGraph :: Map Module (Set Module)
    }
  deriving (Show)

$(makeLenses ''ModuleGraph)

emptyModuleGraph :: ModuleGraph
emptyModuleGraph = MkModuleGraph mempty

insertModuleEdge :: (Module, Module) -> ModuleGraph -> ModuleGraph
insertModuleEdge (parent, child)
  = over moduleGraph (Map.insertWith Set.union parent (Set.singleton child))

computeModuleGraph :: Makefile -> Maybe ModuleGraph
computeModuleGraph = fmap rulesToGraph . entriesToRules . entries
  where
    rulesToGraph :: [(Target, [Target])] -> ModuleGraph
    rulesToGraph = MkModuleGraph
                   . Map.fromListWith Set.union
                   . map (tgtToModule *** (Set.fromList . map tgtToModule))

    tgtToModule :: Target -> Module
    tgtToModule (Target name) = MkModule name

    entriesToRules :: [Entry] -> Maybe [(Target, [Target])]
    entriesToRules = mapM getRule

    getRule :: Entry -> Maybe (Target, [Target])
    getRule entry = do
      (tgt, deps, []) <- fromEntry entry
      pure (tgt, map depToTarget deps)

    fromEntry :: Entry -> Maybe (Target, [Dependency], [Command])
    fromEntry (Rule tgt deps cmds) = Just (tgt, deps, cmds)
    fromEntry _                    = Nothing

    depToTarget :: Dependency -> Target
    depToTarget (Dependency name) = Target name

prettyModuleGraph :: ModuleGraph -> Text
prettyModuleGraph = mconcat
                    . map ((<> "\n") . prettyModuleEdge)
                    . Map.toList
                    . Lens.view moduleGraph
  where
    prettyModuleEdge :: (Module, Set Module) -> Text
    prettyModuleEdge (mod, deps)
      = mconcat [prettyModule mod, " -> ", prettyModuleSet deps, ";"]

    prettyModuleSet :: Set Module -> Text
    prettyModuleSet = wrapBraces
                      . T.intercalate ", "
                      . map prettyModule
                      . Set.toList

    prettyModule :: Module -> Text
    prettyModule (MkModule name) = "\"" <> name <> "\""
                                   -- withAnsi ansiBold name

    wrapBraces :: Text -> Text
    wrapBraces t = mconcat ["{", t, "}"]

    withAnsi :: Int -> Text -> Text
    withAnsi code text = mconcat [ansi code, text, ansi ansiReset]

    ansi :: Int -> Text
    ansi code = mconcat ["\ESC[", T.pack (show code), "m"]

    ansiReset, ansiBold :: Int
    ansiReset = 0
    ansiBold  = 1

printModuleGraph :: ModuleGraph -> IO ()
printModuleGraph = T.putStrLn . prettyModuleGraph

newtype BuildGraph
  = MkBuildGraph
    { fromBuildGraph :: Map Target (Set Target, [Command])
    }

computeBuildGraph :: ModuleGraph -> BuildGraph
computeBuildGraph = _ -- FIXME

testMakefile :: IO Makefile
testMakefile = Makefile.parseAsMakefile "./profunctors/src/Makefile"
               >>= either fail pure

main :: IO ()
main = printModuleGraph . fromJust . computeModuleGraph =<< testMakefile
