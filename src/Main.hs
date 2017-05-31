{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

-- import           Control.Lens        as Lens

import qualified Data.Makefile              as Makefile
import qualified Data.Makefile.Parse        as Makefile

-- import           Turtle

import           Control.Arrow
import           Data.Either
import           Data.Maybe
import           Data.Monoid

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map

import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

import qualified Ninja.All                  as Ninja
import qualified Ninja.Env                  as Ninja
import qualified Ninja.Lexer                as Ninja
import qualified Ninja.Parse                as Ninja
import qualified Ninja.Pretty               as Ninja
import qualified Ninja.Type                 as Ninja

import           Ninja.Eval
                 (Command (..), RuleName (..), Target (..))

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           Hash

--------------------------------------------------------------------------------

pretty :: (ToJSON v) => v -> IO ()
pretty = LBS8.putStrLn . encodePretty

debugNinja :: IO Ninja.Ninja
debugNinja = Ninja.parse "../data/build.ninja"

debugSNinja :: IO SNinja
debugSNinja = compileNinja <$> debugNinja

--------------------------------------------------------------------------------

targetFromBS :: ByteString -> Target
targetFromBS = MkTarget . T.decodeUtf8

ruleNameFromBS :: ByteString -> RuleName
ruleNameFromBS = MkRuleName . T.decodeUtf8

commandFromBS :: ByteString -> Command
commandFromBS = MkCommand . T.decodeUtf8

-- | A simplified build graph.
data SNinja
  = MkSNinja
    { _snBuilds   :: HashMap (HashSet Target) SBuild
    , _snDefaults :: HashSet Target
    }
  deriving (Eq, Show)

-- | Lint the given build graph.
checkSNinja :: SNinja -> Bool
checkSNinja (MkSNinja builds defaults) = and [ noOverlappingOutputs
                                             ]
  where
    noOverlappingOutputs = let outs = HM.keys builds
                           in HS.size (mconcat outs) == sum (map HS.size outs)

-- | Look up the unique build rule that outputs the given target.
lookupBuild :: SNinja -> Target -> Maybe (HashSet Target, SBuild)
lookupBuild (MkSNinja builds _) target
  = case HM.toList (HM.filterWithKey (\k _ -> HS.member target k) builds)
    of [result] -> Just result
       []       -> Nothing
       _        -> error "invalid SNinja: overlapping keys"

-- | Compute the set of all targets referenced in the build graph.
allTargets :: SNinja -> HashSet Target
allTargets (MkSNinja builds defaults) = outputs <> defaults <> dependencies
  where
    outputs, dependencies :: HashSet Target
    outputs      = mconcat $ HM.keys builds
    dependencies = mconcat $ map (_sbDeps . snd) $ HM.toList builds

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
    outputs = mconcat $ HM.keys builds

-- | Compute the set of targets that the given target depends on.
targetReferences :: SNinja -> Target -> HashSet Target
targetReferences (sn@(MkSNinja builds _)) target
  = case lookupBuild sn target
    of Just (_, MkSBuild _ deps) -> deps
       Nothing                   -> HS.empty

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
compileNinja :: Ninja.Ninja -> SNinja
compileNinja (Ninja.MkNinja {..}) = MkSNinja simpleBuilds simpleDefaults
  where
    simpleBuilds :: HashMap (HashSet Target) SBuild
    simpleBuilds = HM.map simplifyBuild combined
                   <> HM.fromList (map simplifyPhony phonys)

    simpleDefaults :: HashSet Target
    simpleDefaults = HS.map targetFromBS $ HS.fromList defaults

    simplifyBuild :: Ninja.Build -> SBuild
    simplifyBuild (Ninja.MkBuild {..}) = MkSBuild (Just rule) deps
      where
        deps = HS.map targetFromBS $ HS.fromList $ mconcat
               [depsNormal, depsImplicit, depsOrderOnly]

        rule = fromMaybe (error ("rule not found: " <> show ruleName))
               $ HM.lookup (ruleNameFromBS ruleName) ruleMap

    simplifyPhony :: (ByteString, [ByteString]) -> (HashSet Target, SBuild)
    simplifyPhony (name, files) = ( HS.singleton (targetFromBS name)
                                  , MkSBuild Nothing (HS.fromList filesT) )
      where
        filesT = map targetFromBS files

    ruleMap :: HashMap RuleName Command
    ruleMap = HM.fromList $ map (ruleNameFromBS *** computeCommand) rules

    computeCommand :: Ninja.Rule -> Command
    computeCommand (Ninja.MkRule {..})
      = case lookup "command" ruleBind
        of Just (Ninja.Lit x) -> commandFromBS x
           Just _             -> error "rule uses variables"
           Nothing            -> error "\"command\" not found"

    combined :: HashMap (HashSet Target) Ninja.Build
    combined = HM.fromList
               $ map (first (HS.fromList . map targetFromBS))
               $ multiples <> map (first (\x -> [x])) singles

newtype Path
  = MkPath Text
  deriving ()

data DerivationType
  = NormalDerivation
  | FixedOutputDerivation !(Digest SHA256)
  deriving ()

data Derivation
  = MkDerivation
    { _drvName    :: !Text
    , _drvOutputs :: !(HashSet Text)
    , _drvBuilder :: !Path
    , _drvArgs    :: ![Text]
    , _drvInputs  :: !(HashSet Derivation)
    , _drvEnv     :: !(HashMap Text Text)
    , _drvType    :: !DerivationType
    }
  deriving ()

main :: IO ()
main = pure ()

instance ToJSON SNinja where
  toJSON (MkSNinja {..}) = object ["builds" .= builds, "defaults" .= defaults]
    where
      builds, defaults :: Value
      builds   = toJSON $ map buildPair $ HM.toList _snBuilds
      defaults = toJSON _snDefaults

      buildPair :: (HashSet Target, SBuild) -> Value
      buildPair (outs, build) = object ["outputs" .= outs, "build" .= build]

instance ToJSON SBuild where
  toJSON (MkSBuild Nothing  deps) = object ["phony" .= deps]
  toJSON (MkSBuild (Just c) deps) = object ["cmd" .= c, "deps" .= deps]
