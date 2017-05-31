{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | FIXME: doc
module Ninja.Eval
  ( module Ninja.Eval -- FIXME: specific export list
  ) where

import           Ninja.Type            (FileStr, Str)
import qualified Ninja.Type            as Ninja

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (unlines, unwords)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import           Data.Hashable         (Hashable)

import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM

import           Data.Aeson            as Aeson
import qualified Data.Aeson.Types      as Aeson

-- | FIXME: doc
newtype PoolName
  = MkPoolName Text
  deriving (Eq, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | FIXME: doc
newtype Target
  = MkTarget Text
  deriving (Eq, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | FIXME: doc
newtype RuleName
  = MkRuleName Text
  deriving (Eq, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | This type represents a command with arguments.
data Command
  = MkCommand
    { _commandExec :: Text
      -- ^ The command to run.
      --   If this is an absolute path to an executable file, the file will be
      --   executed. Otherwise, it will be treated as a command name that will
      --   be searched for in the @PATH@ environment variable.
    , _commandArgs :: [Text]
      -- ^ The arguments for the command.
    }
  deriving (Eq)

-- | FIXME: doc
data ENinja
  = MkENinja
    { _ninjaRules    :: !(HashMap RuleName ERule)
      -- ^ FIXME: doc
    , _ninjaBuilds   :: !(HashMap [Target] EBuild)
      -- ^ FIXME: doc
    , _ninjaPhonys   :: !(HashMap Target [Target])
      -- ^ FIXME: doc
    , _ninjaDefaults :: ![Target]
      -- ^ FIXME: doc
    , _ninjaPools    :: !(HashMap PoolName Int)
      -- ^ FIXME: doc
    }
  deriving ()

instance ToJSON ENinja where
  toJSON (MkENinja {..}) = object
                           [ "rules"    .= rules
                           , "builds"   .= builds
                           , "phonys"   .= phonys
                           , "defaults" .= defaults
                           , "pools"    .= pools
                           ]
    where
      rules, builds, phonys, defaults, pools :: Value
      rules    = toJSON _ninjaRules
      builds   = toJSON $ map buildPair $ HM.toList _ninjaBuilds
      phonys   = toJSON _ninjaPhonys
      defaults = toJSON _ninjaDefaults
      pools    = toJSON _ninjaPools

      buildPair :: ([Target], EBuild) -> Value
      buildPair (outs, build) = object ["outputs" .= outs, "build" .= build]

instance FromJSON ENinja where
  parseJSON = withObject "ENinja" $ \o -> MkENinja
                                          <$> (o .: "rules"    >>= rulesP)
                                          <*> (o .: "builds"   >>= buildsP)
                                          <*> (o .: "phonys"   >>= phonysP)
                                          <*> (o .: "defaults" >>= defaultsP)
                                          <*> (o .: "pools"    >>= poolsP)
    where
      rulesP    :: Value -> Aeson.Parser (HashMap RuleName ERule)
      rulesP    = parseJSON
      buildsP   :: Value -> Aeson.Parser (HashMap [Target] EBuild)
      buildsP   = let seqMap f = sequenceA . map f
                  in \v -> HM.fromList <$> (parseJSON v >>= seqMap buildPairP)
      phonysP   :: Value -> Aeson.Parser (HashMap Target [Target])
      phonysP   = parseJSON
      defaultsP :: Value -> Aeson.Parser [Target]
      defaultsP = parseJSON
      poolsP    :: Value -> Aeson.Parser (HashMap PoolName Int)
      poolsP    = parseJSON

      buildPairP :: Value -> Aeson.Parser ([Target], EBuild)
      buildPairP = withObject "ENinja"
                   $ \o -> (,) <$> (o .: "outputs") <*> (o .: "build")

-- | FIXME: doc
data ERule
  = MkERule
    { _ruleCommand      :: !Command
      -- ^ The command that this rule will run.
    , _ruleDepfile      :: !(Maybe FileStr)
      -- ^ If set, this should be a path to an optional Makefile that contains
      --   extra implicit dependencies. This is used to support C/C++ header
      --   dependencies.
    , _ruleDeps         :: !(Maybe Deps)
      -- ^ If set, enables special dependency processing used in C/C++ header
      --   dependencies. For more information, read the Ninja documentation
      --   <https://ninja-build.org/manual.html#ref_headers here>.
    , _ruleDescription  :: !(Maybe Text)
      -- ^ A short description of the command, used to pretty-print the command
      --   as it's running. The @ninja -v@ flag controls whether to print the
      --   full command or its description; if a command fails, the full command
      --   line will always be printed before the command's output.
    , _ruleGenerator    :: !Bool
      -- ^ If this is true, specifies that this rule is used to re-invoke the
      --   generator program. Files built using generator rules are treated
      --   specially in two ways: firstly, they will not be rebuilt if the
      --   command line changes; and secondly, they are not cleaned by default.
    , _ruleResponseFile :: !(Maybe (FileStr, Str))
      -- ^ If present, Ninja will use a response file for the given command,
      --   i.e. write the selected string to the given file before calling the
      --   command and delete the file after the command is done.
      --
      --   This is particularly useful on Windows OS, where the maximal length
      --   of a command line is limited and response files must be used instead.
    }
  deriving (Eq)

instance ToJSON ERule where
  toJSON = undefined -- FIXME

instance FromJSON ERule where
  parseJSON = undefined -- FIXME

-- | FIXME: doc
makeRule :: Command
         -- ^ The command to run.
         -> ERule
         -- ^ A rule that runs this command.
makeRule cmd = MkERule
               { _ruleCommand      = cmd
               , _ruleDepfile      = Nothing
               , _ruleDeps         = Nothing
               , _ruleDescription  = Nothing
               , _ruleGenerator    = False
               , _ruleResponseFile = Nothing
               }

-- | FIXME: doc
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_headers here>.
data Deps
  = DepsGCC
    -- ^ Special dependency processing for GCC.
  | DepsMSVC
    -- ^ Special dependency processing for MSVC.
    { _depsMSVCPrefix :: !(Maybe Text)
      -- ^ This defines the string which should be stripped from @msvc@'s
      --   @/showIncludes@ output. Only needed if the version of Visual Studio
      --   being used is not English.
    }
  deriving (Eq)

instance ToJSON Deps where
  toJSON = go
    where
      go DepsGCC             = object ["deps" .= gcc]
      go (DepsMSVC Nothing)  = object ["deps" .= msvc]
      go (DepsMSVC (Just p)) = object ["deps" .= msvc, "prefix" .= p]

      gcc, msvc :: Value
      gcc = "gcc"
      msvc = "msvc"

instance FromJSON Deps where
  parseJSON = undefined -- FIXME

depsGCC :: Deps
depsGCC = DepsGCC

depsMSVC :: Deps
depsMSVC = DepsMSVC Nothing

data EBuild
  = MkEBuild
    {
    }
  deriving ()

instance ToJSON EBuild where
  toJSON = undefined -- FIXME

instance FromJSON EBuild where
  parseJSON = undefined -- FIXME
