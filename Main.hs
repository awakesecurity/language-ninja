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
prettyModuleGraph = \mg -> mconcat
                           [ "digraph {\n"
                           , prettyModuleEdges mg
                           , "}\n"
                           ]
  where
    prettyModuleEdges :: ModuleGraph -> Text
    prettyModuleEdges = mconcat
                        . map ((\x -> "  " <> x <> "\n") . prettyModuleEdge)
                        . Map.toList
                        . Lens.view moduleGraph

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

newtype Path
  = MkPath Text
  deriving (Eq, Ord, Show)

newtype BuildGraph
  = MkBuildGraph
    { fromBuildGraph :: Map Path (Set Path, [Command])
    }

computeBuildGraph :: ModuleGraph -> BuildGraph
computeBuildGraph = \mg -> makeBuildGraph
                           $ map (convertNode mg)
                           $ Map.toList (mg ^. moduleGraph)
  where
    makeBuildGraph :: [(Path, (Set Path, [Command]))] -> BuildGraph
    makeBuildGraph = MkBuildGraph . Map.fromList

    convertNode :: ModuleGraph
                -> (Module, Set Module)
                -> (Path, (Set Path, [Command]))
    convertNode mg (mod, deps) = ( modToPath mod
                                 , ( generateDependencies mg mod deps
                                   , generateCommands     mg mod deps )
                                 )

    modToPath :: Module -> Path
    modToPath (MkModule name) = MkPath name

    generateDependencies :: ModuleGraph -> Module -> Set Module -> Set Path
    generateDependencies mg mod deps = Set.union (Set.map modToPath deps)
                                       $ if isObjectFile mod
                                         then objectFileDependencies mg mod deps
                                         else Set.empty

    generateCommands :: ModuleGraph -> Module -> Set Module -> [Command]
    generateCommands mg mod deps = if isObjectFile mod
                                   then objectFileCommands mg mod deps
                                   else []

    isObjectFile :: Module -> Bool
    isObjectFile (MkModule m) = (T.takeEnd 2 m == ".o")

    objectFileCommands :: ModuleGraph -> Module -> Set Module -> [Command]
    objectFileCommands mg mod deps = [""] -- FIXME

    objectFileDependencies :: ModuleGraph -> Module -> Set Module -> Set Path
    objectFileDependencies = _

testMakefile :: IO Makefile
testMakefile = Makefile.parseAsMakefile "./profunctors/src/Makefile"
               >>= either fail pure

main :: IO ()
main = printModuleGraph . fromJust . computeModuleGraph =<< testMakefile
