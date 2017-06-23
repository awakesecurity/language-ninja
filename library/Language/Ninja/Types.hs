-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Types.hs
--
-- License:
--     Copyright Neil Mitchell 2011-2017.
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions are
--     met:
--
--         * Redistributions of source code must retain the above copyright
--           notice, this list of conditions and the following disclaimer.
--
--         * Redistributions in binary form must reproduce the above
--           copyright notice, this list of conditions and the following
--           disclaimer in the documentation and/or other materials provided
--           with the distribution.
--
--         * Neither the name of Neil Mitchell nor the names of other
--           contributors may be used to endorse or promote products derived
--           from this software without specific prior written permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- |
--   Module      : Language.Ninja.Types
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The IO in this module is only to evaluate an environment variable,
--   the 'Env' itself is passed around purely.
module Language.Ninja.Types
  ( -- * @PNinja@
    PNinja, makePNinja
  , pninjaRules
  , pninjaSingles
  , pninjaMultiples
  , pninjaPhonys
  , pninjaDefaults
  , pninjaPools
  , pninjaSpecials

    -- * @PBuild@
  , PBuild, makePBuild
  , pbuildRule, pbuildEnv, pbuildDeps, pbuildBind

    -- * @PDeps@
  , PDeps, makePDeps
  , pdepsNormal, pdepsImplicit, pdepsOrderOnly

    -- * @PRule@
  , PRule, makePRule
  , pruleBind

    -- * @PExpr@
  , PExpr (..), askVar, askExpr, addBind, addBinds

    -- * @Env@
  , Env, Ninja.makeEnv, Ninja.fromEnv, Ninja.addEnv, Ninja.scopeEnv

    -- * Miscellaneous
  , Str, FileStr, Text, FileText
  ) where

import           Control.Arrow             (second)
import           Control.Monad             ((>=>))

import           Control.Lens.Lens         (Lens')
import qualified Control.Lens

import           Data.Foldable             (asum)
import qualified Data.Maybe
import           Data.Monoid               (Endo(..))

import qualified Data.ByteString.Char8     as BSC8

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS

import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import           Data.Aeson                (FromJSON(..), KeyValue(..),
                                            ToJSON(..), Value, (.:))
import qualified Data.Aeson.Types          as Aeson

import qualified Test.QuickCheck           as Q
import           Test.QuickCheck.Arbitrary (Arbitrary(..))
import           Test.QuickCheck.Gen       (Gen(..))
import           Test.QuickCheck.Instances ()

import           Language.Ninja.Env        (Env)
import qualified Language.Ninja.Env        as Ninja

import           Flow                      ((|>), (.>))

--------------------------------------------------------------------------------

-- | FIXME: doc
type Str = BSC8.ByteString

-- | FIXME: doc
type FileStr = BSC8.ByteString

-- | FIXME: doc
type FileText = Text

--------------------------------------------------------------------------------

-- | FIXME: doc
data PExpr
  = -- | FIXME: doc
    PExprs [PExpr]
  | -- | FIXME: doc
    PLit Text
  | -- | FIXME: doc
    PVar Text
  deriving (Eq, Show, Generic)

-- | FIXME: doc
askExpr :: Env Text Text -> PExpr -> Text
askExpr e (PExprs xs) = T.concat (map (askExpr e) xs)
askExpr _ (PLit x)    = x
askExpr e (PVar x)    = askVar e x

-- | FIXME: doc
askVar :: Env Text Text -> Text -> Text
askVar e x = Data.Maybe.fromMaybe T.empty (Ninja.askEnv e x)

-- | FIXME: doc
addBind :: Text -> PExpr -> Env Text Text -> Env Text Text
addBind k v e = Ninja.addEnv k (askExpr e v) e

-- | FIXME: doc
addBinds :: [(Text, PExpr)] -> Env Text Text -> Env Text Text
addBinds bs e = map (second (askExpr e) .> uncurry Ninja.addEnv .> Endo) bs
                |> mconcat
                |> (\endo -> appEndo endo e)

-- | FIXME: doc
instance ToJSON PExpr where
  toJSON (PExprs xs) = toJSON xs
  toJSON (PLit  str) = toJSON str
  toJSON (PVar  var) = Aeson.object ["var" .= var]

-- | FIXME: doc
instance FromJSON PExpr where
  parseJSON = [ \v -> PExprs <$> parseJSON v
              , \v -> PLit   <$> parseJSON v
              , Aeson.withObject "PExpr" $ \o -> PVar <$> (o .: "var")
              ] |> choice
    where
      choice :: [Value -> Aeson.Parser a] -> (Value -> Aeson.Parser a)
      choice = flip (\v -> map (\f -> f v)) .> fmap asum

-- | FIXME: doc
instance Arbitrary PExpr where
  arbitrary = Q.sized go
    where
      go :: Int -> Gen PExpr
      go n | n <= 0 = [ PLit <$> Q.resize litLength arbitrary
                      , PVar <$> Q.resize varLength arbitrary
                      ] |> Q.oneof
      go n          = [ go 0
                      , do width <- (`mod` maxWidth) <$> arbitrary
                           let subtree = go (n `div` lossRate)
                           PExprs <$> Q.vectorOf width subtree
                      ] |> Q.oneof

      litLength, varLength, lossRate, maxWidth :: Int
      litLength = 10
      varLength = 10
      maxWidth  = 5
      lossRate  = 2

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data PNinja
  = MkPNinja
    { _pninjaRules     :: !(HashMap Text PRule)
    , _pninjaSingles   :: !(HashMap FileText PBuild)
    , _pninjaMultiples :: !(HashMap (HashSet FileText) PBuild)
    , _pninjaPhonys    :: !(HashMap Text (HashSet FileText))
    , _pninjaDefaults  :: !(HashSet FileText)
    , _pninjaPools     :: !(HashMap Text Int)
    , _pninjaSpecials  :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'PNinja' with all default values
makePNinja :: PNinja
makePNinja = MkPNinja
             { _pninjaRules     = mempty
             , _pninjaSingles   = mempty
             , _pninjaMultiples = mempty
             , _pninjaPhonys    = mempty
             , _pninjaDefaults  = mempty
             , _pninjaPools     = mempty
             , _pninjaSpecials  = mempty
             }

-- | The rules defined in a parsed Ninja file.
pninjaRules :: Lens' PNinja (HashMap Text PRule)
pninjaRules = Control.Lens.lens _pninjaRules
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
pninjaSingles :: Lens' PNinja (HashMap FileText PBuild)
pninjaSingles = Control.Lens.lens _pninjaSingles
                $ \(MkPNinja {..}) x -> MkPNinja { _pninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
pninjaMultiples :: Lens' PNinja (HashMap (HashSet FileText) PBuild)
pninjaMultiples = Control.Lens.lens _pninjaMultiples
                  $ \(MkPNinja {..}) x -> MkPNinja { _pninjaMultiples = x, .. }

-- | The set of phony build declarations.
pninjaPhonys :: Lens' PNinja (HashMap Text (HashSet FileText))
pninjaPhonys = Control.Lens.lens _pninjaPhonys
               $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPhonys = x, .. }

-- | The set of default targets.
pninjaDefaults :: Lens' PNinja (HashSet FileText)
pninjaDefaults = Control.Lens.lens _pninjaDefaults
                 $ \(MkPNinja {..}) x -> MkPNinja { _pninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
pninjaPools :: Lens' PNinja (HashMap Text Int)
pninjaPools = Control.Lens.lens _pninjaPools
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPools = x, .. }

-- | A map from "special" top-level variables to their values.
pninjaSpecials :: Lens' PNinja (HashMap Text Text)
pninjaSpecials = Control.Lens.lens _pninjaSpecials
                 $ \(MkPNinja {..}) x -> MkPNinja { _pninjaSpecials = x, .. }

-- | FIXME: doc
instance ToJSON PNinja where
  toJSON (MkPNinja {..})
    = [ "rules"     .= _pninjaRules
      , "singles"   .= _pninjaSingles
      , "multiples" .= fixMultiples _pninjaMultiples
      , "phonys"    .= _pninjaPhonys
      , "defaults"  .= _pninjaDefaults
      , "pools"     .= _pninjaPools
      , "specials"  .= _pninjaPools
      ] |> Aeson.object
    where
      fixMultiples :: HashMap (HashSet FileText) PBuild -> Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> toJSON

      printPair :: HashSet FileText -> PBuild -> Value
      printPair outputs build =
        Aeson.object ["outputs" .= outputs, "build" .= build]

-- | FIXME: doc
instance FromJSON PNinja where
  parseJSON = (Aeson.withObject "PNinja" $ \o -> do
                  _pninjaRules     <- (o .: "rules")     >>= pure
                  _pninjaSingles   <- (o .: "singles")   >>= pure
                  _pninjaMultiples <- (o .: "multiples") >>= fixMultiples
                  _pninjaPhonys    <- (o .: "phonys")    >>= pure
                  _pninjaDefaults  <- (o .: "defaults")  >>= pure
                  _pninjaPools     <- (o .: "pools")     >>= pure
                  _pninjaSpecials  <- (o .: "specials")  >>= pure
                  pure (MkPNinja {..}))
    where
      fixMultiples :: Value -> Aeson.Parser (HashMap (HashSet FileText) PBuild)
      fixMultiples = parseJSON >=> mapM parsePair >=> (HM.fromList .> pure)

      parsePair :: Value -> Aeson.Parser (HashSet FileText, PBuild)
      parsePair = (Aeson.withObject "PNinja.multiples" $ \o -> do
                      outputs <- (o .: "outputs") >>= pure
                      build   <- (o .: "build")   >>= pure
                      pure (outputs, build))

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data PBuild
  = MkPBuild
    { _pbuildRule :: !Text
    , _pbuildEnv  :: !(Env Text Text)
    , _pbuildDeps :: !PDeps
    , _pbuildBind :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'PBuild' with all default values.
makePBuild :: Text
           -- ^ The rule name
           -> Env Text Text
           -- ^ The environment
           -> PBuild
makePBuild rule env = MkPBuild
                      { _pbuildRule = rule
                      , _pbuildEnv  = env
                      , _pbuildDeps = makePDeps
                      , _pbuildBind = mempty
                      }

-- | A lens into the rule name associated with a 'PBuild'.
pbuildRule :: Lens' PBuild Text
pbuildRule = Control.Lens.lens _pbuildRule
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildRule = x, .. }

-- | A lens into the environment associated with a 'PBuild'.
pbuildEnv :: Lens' PBuild (Env Text Text)
pbuildEnv = Control.Lens.lens _pbuildEnv
            $ \(MkPBuild {..}) x -> MkPBuild { _pbuildEnv = x, .. }

-- | A lens into the dependencies associated with a 'PBuild'.
pbuildDeps :: Lens' PBuild PDeps
pbuildDeps = Control.Lens.lens _pbuildDeps
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildDeps = x, .. }

-- | A lens into the bindings associated with a 'PBuild'.
pbuildBind :: Lens' PBuild (HashMap Text Text)
pbuildBind = Control.Lens.lens _pbuildBind
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildBind = x, .. }

-- | FIXME: doc
instance ToJSON PBuild where
  toJSON (MkPBuild {..})
    = [ "rule" .= _pbuildRule
      , "env"  .= _pbuildEnv
      , "deps" .= _pbuildDeps
      , "bind" .= _pbuildBind
      ] |> Aeson.object

-- | FIXME: doc
instance FromJSON PBuild where
  parseJSON = (Aeson.withObject "PBuild" $ \o -> do
                  _pbuildRule <- (o .: "rule") >>= pure
                  _pbuildEnv  <- (o .: "env")  >>= pure
                  _pbuildDeps <- (o .: "deps") >>= pure
                  _pbuildBind <- (o .: "bind") >>= pure
                  pure (MkPBuild {..}))

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
data PDeps
  = MkPDeps
    { _pdepsNormal    :: !(HashSet FileText)
    , _pdepsImplicit  :: !(HashSet FileText)
    , _pdepsOrderOnly :: !(HashSet FileText)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'PDeps' with all default values
makePDeps :: PDeps
makePDeps = MkPDeps
            { _pdepsNormal    = mempty
            , _pdepsImplicit  = mempty
            , _pdepsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'PDeps'.
pdepsNormal :: Lens' PDeps (HashSet FileText)
pdepsNormal = Control.Lens.lens _pdepsNormal
              $ \(MkPDeps {..}) x -> MkPDeps { _pdepsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'PDeps'.
pdepsImplicit :: Lens' PDeps (HashSet FileText)
pdepsImplicit = Control.Lens.lens _pdepsImplicit
                $ \(MkPDeps {..}) x -> MkPDeps { _pdepsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'PDeps'.
pdepsOrderOnly :: Lens' PDeps (HashSet FileText)
pdepsOrderOnly = Control.Lens.lens _pdepsOrderOnly
                 $ \(MkPDeps {..}) x -> MkPDeps { _pdepsOrderOnly = x, .. }

-- | FIXME: doc
instance ToJSON PDeps where
  toJSON (MkPDeps {..})
    = [ "normal"     .= _pdepsNormal
      , "implicit"   .= _pdepsImplicit
      , "order-only" .= _pdepsOrderOnly
      ] |> Aeson.object

-- | FIXME: doc
instance FromJSON PDeps where
  parseJSON = (Aeson.withObject "PDeps" $ \o -> do
                  _pdepsNormal    <- (o .: "normal")     >>= pure
                  _pdepsImplicit  <- (o .: "implicit")   >>= pure
                  _pdepsOrderOnly <- (o .: "order-only") >>= pure
                  pure (MkPDeps {..}))

--------------------------------------------------------------------------------

-- | A parsed Ninja @rule@ declaration.
newtype PRule
  = MkPRule
    { _pruleBind :: HashMap Text PExpr
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'PRule' with all default values
makePRule :: PRule
makePRule = MkPRule
            { _pruleBind = mempty
            }

-- | The set of bindings in scope during the execution of this rule.
pruleBind :: Lens' PRule (HashMap Text PExpr)
pruleBind = Control.Lens.lens _pruleBind (const MkPRule)

-- | FIXME: doc
instance ToJSON PRule where
  toJSON = _pruleBind .> toJSON

-- | FIXME: doc
instance FromJSON PRule where
  parseJSON = parseJSON .> fmap MkPRule

--------------------------------------------------------------------------------
