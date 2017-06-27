-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST.hs
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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: split this module up
module Language.Ninja.AST
  ( -- * @Ninja@
    Ninja, makeNinja
  , ninjaRules
  , ninjaSingles
  , ninjaMultiples
  , ninjaPhonys
  , ninjaDefaults
  , ninjaPools
  , ninjaSpecials
  , NinjaConstraint

    -- * @PBuild@
  , PBuild, makePBuild
  , pbuildRule, pbuildEnv, pbuildDeps, pbuildBind
  , PBuildConstraint

    -- * @PDeps@
  , PDeps, makePDeps
  , pdepsNormal, pdepsImplicit, pdepsOrderOnly

    -- * @Rule@
  , AST.Rule, AST.makeRule
  , AST.ruleBind

    -- * @Expr@
  , AST.Expr (..)
  , AST._Exprs, AST._Lit, AST._Var
  , AST.askVar, AST.askExpr, AST.addBind, AST.addBinds

    -- * @Env@
  , AST.Env
  , AST.makeEnv, AST.fromEnv, AST.addEnv, AST.scopeEnv

    -- * Miscellaneous
  , Str, FileStr, Text, FileText
  ) where

import           Control.Arrow           (second)
import           Control.Monad           ((>=>))

import qualified Control.Lens
import           Control.Lens.Lens       (Lens')
import           Control.Lens.Lens
import           Control.Lens.Prism

import           Data.Foldable           (asum)
import qualified Data.Maybe
import           Data.Monoid             (Endo (..))

import qualified Data.ByteString.Char8   as BSC8

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM

import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HS

import           Data.Hashable           (Hashable)
import           GHC.Generics            (Generic)

import           GHC.Exts                (Constraint)

import           Data.Aeson
                 (FromJSON (..), KeyValue (..), ToJSON (..), Value, (.:))
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson

import qualified Test.SmallCheck.Series  as SC

import qualified Language.Ninja.AST.Env  as AST
import qualified Language.Ninja.AST.Expr as AST
import qualified Language.Ninja.AST.Rule as AST

import           Flow                    ((.>), (|>))

--------------------------------------------------------------------------------

-- | A type alias for 'BSC8.ByteString'.
type Str = BSC8.ByteString

-- | A type alias for 'BSC8.ByteString', representing a path.
type FileStr = BSC8.ByteString

-- | A type alias for 'Text', representing a path.
type FileText = Text

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data Ninja
  = MkNinja
    { _ninjaRules     :: !(HashMap Text AST.Rule)
    , _ninjaSingles   :: !(HashMap FileText PBuild)
    , _ninjaMultiples :: !(HashMap (HashSet FileText) PBuild)
    , _ninjaPhonys    :: !(HashMap Text (HashSet FileText))
    , _ninjaDefaults  :: !(HashSet FileText)
    , _ninjaPools     :: !(HashMap Text Int)
    , _ninjaSpecials  :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Ninja' with all default values
{-# INLINE makeNinja #-}
makeNinja :: Ninja
makeNinja = MkNinja
            { _ninjaRules     = mempty
            , _ninjaSingles   = mempty
            , _ninjaMultiples = mempty
            , _ninjaPhonys    = mempty
            , _ninjaDefaults  = mempty
            , _ninjaPools     = mempty
            , _ninjaSpecials  = mempty
            }

-- | The rules defined in a parsed Ninja file.
{-# INLINE ninjaRules #-}
ninjaRules :: Lens' Ninja (HashMap Text AST.Rule)
ninjaRules = Control.Lens.lens _ninjaRules
             $ \(MkNinja {..}) x -> MkNinja { _ninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
{-# INLINE ninjaSingles #-}
ninjaSingles :: Lens' Ninja (HashMap FileText PBuild)
ninjaSingles = Control.Lens.lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens' Ninja (HashMap (HashSet FileText) PBuild)
ninjaMultiples = Control.Lens.lens _ninjaMultiples
                 $ \(MkNinja {..}) x -> MkNinja { _ninjaMultiples = x, .. }

-- | The set of phony build declarations.
{-# INLINE ninjaPhonys #-}
ninjaPhonys :: Lens' Ninja (HashMap Text (HashSet FileText))
ninjaPhonys = Control.Lens.lens _ninjaPhonys
              $ \(MkNinja {..}) x -> MkNinja { _ninjaPhonys = x, .. }

-- | The set of default targets.
{-# INLINE ninjaDefaults #-}
ninjaDefaults :: Lens' Ninja (HashSet FileText)
ninjaDefaults = Control.Lens.lens _ninjaDefaults
                $ \(MkNinja {..}) x -> MkNinja { _ninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
{-# INLINE ninjaPools #-}
ninjaPools :: Lens' Ninja (HashMap Text Int)
ninjaPools = Control.Lens.lens _ninjaPools
             $ \(MkNinja {..}) x -> MkNinja { _ninjaPools = x, .. }

-- | A map from "special" top-level variables to their values.
{-# INLINE ninjaSpecials #-}
ninjaSpecials :: Lens' Ninja (HashMap Text Text)
ninjaSpecials = Control.Lens.lens _ninjaSpecials
                $ \(MkNinja {..}) x -> MkNinja { _ninjaSpecials = x, .. }

-- | Converts to
--   @{rules: …, singles: …, multiples: …, phonys: …, defaults: …,
--     pools: …, specials: …}@.
instance ToJSON Ninja where
  toJSON (MkNinja {..})
    = [ "rules"     .= _ninjaRules
      , "singles"   .= _ninjaSingles
      , "multiples" .= fixMultiples _ninjaMultiples
      , "phonys"    .= _ninjaPhonys
      , "defaults"  .= _ninjaDefaults
      , "pools"     .= _ninjaPools
      , "specials"  .= _ninjaPools
      ] |> Aeson.object
    where
      fixMultiples :: HashMap (HashSet FileText) PBuild -> Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> toJSON

      printPair :: HashSet FileText -> PBuild -> Value
      printPair outputs build =
        Aeson.object ["outputs" .= outputs, "build" .= build]

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Ninja where
  parseJSON = (Aeson.withObject "Ninja" $ \o -> do
                  _ninjaRules     <- (o .: "rules")     >>= pure
                  _ninjaSingles   <- (o .: "singles")   >>= pure
                  _ninjaMultiples <- (o .: "multiples") >>= fixMultiples
                  _ninjaPhonys    <- (o .: "phonys")    >>= pure
                  _ninjaDefaults  <- (o .: "defaults")  >>= pure
                  _ninjaPools     <- (o .: "pools")     >>= pure
                  _ninjaSpecials  <- (o .: "specials")  >>= pure
                  pure (MkNinja {..}))
    where
      fixMultiples :: Value -> Aeson.Parser (HashMap (HashSet FileText) PBuild)
      fixMultiples = parseJSON >=> mapM parsePair >=> (HM.fromList .> pure)

      parsePair :: Value -> Aeson.Parser (HashSet FileText, PBuild)
      parsePair = (Aeson.withObject "Ninja.multiples" $ \o -> do
                      outputs <- (o .: "outputs") >>= pure
                      build   <- (o .: "build")   >>= pure
                      pure (outputs, build))

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, NinjaConstraint (SC.Serial m)) => SC.Serial m Ninja

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, NinjaConstraint (SC.CoSerial m)) => SC.CoSerial m Ninja

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Ninja'.
type NinjaConstraint (c :: * -> Constraint)
  = ( PBuildConstraint c
    , c (HashMap (HashSet FileText) PBuild)
    , c (HashMap Text (HashSet FileText))
    , c (HashMap Text AST.Rule)
    , c (HashMap FileText PBuild)
    , c (HashMap Text Int)
    )

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data PBuild
  = MkPBuild
    { _pbuildRule :: !Text
    , _pbuildEnv  :: !(AST.Env Text Text)
    , _pbuildDeps :: !PDeps
    , _pbuildBind :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'PBuild' with all default values.
{-# INLINE makePBuild #-}
makePBuild :: Text
           -- ^ The rule name
           -> AST.Env Text Text
           -- ^ The environment
           -> PBuild
makePBuild rule env = MkPBuild
                      { _pbuildRule = rule
                      , _pbuildEnv  = env
                      , _pbuildDeps = makePDeps
                      , _pbuildBind = mempty
                      }

-- | A lens into the rule name associated with a 'PBuild'.
{-# INLINE pbuildRule #-}
pbuildRule :: Lens' PBuild Text
pbuildRule = Control.Lens.lens _pbuildRule
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildRule = x, .. }

-- | A lens into the environment associated with a 'PBuild'.
{-# INLINE pbuildEnv #-}
pbuildEnv :: Lens' PBuild (AST.Env Text Text)
pbuildEnv = Control.Lens.lens _pbuildEnv
            $ \(MkPBuild {..}) x -> MkPBuild { _pbuildEnv = x, .. }

-- | A lens into the dependencies associated with a 'PBuild'.
{-# INLINE pbuildDeps #-}
pbuildDeps :: Lens' PBuild PDeps
pbuildDeps = Control.Lens.lens _pbuildDeps
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildDeps = x, .. }

-- | A lens into the bindings associated with a 'PBuild'.
{-# INLINE pbuildBind #-}
pbuildBind :: Lens' PBuild (HashMap Text Text)
pbuildBind = Control.Lens.lens _pbuildBind
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildBind = x, .. }

-- | Converts to @{rule: …, env: …, deps: …, bind: …}@.
instance ToJSON PBuild where
  toJSON (MkPBuild {..})
    = [ "rule" .= _pbuildRule
      , "env"  .= _pbuildEnv
      , "deps" .= _pbuildDeps
      , "bind" .= _pbuildBind
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON PBuild where
  parseJSON = (Aeson.withObject "PBuild" $ \o -> do
                  _pbuildRule <- (o .: "rule") >>= pure
                  _pbuildEnv  <- (o .: "env")  >>= pure
                  _pbuildDeps <- (o .: "deps") >>= pure
                  _pbuildBind <- (o .: "bind") >>= pure
                  pure (MkPBuild {..}))

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, PBuildConstraint (SC.Serial m)) => SC.Serial m PBuild

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, PBuildConstraint (SC.CoSerial m)) => SC.CoSerial m PBuild

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'PBuild'.
type PBuildConstraint (c :: * -> Constraint)
  = ( c Text
    , c (HashSet FileText)
    , c (HashMap Text Text)
    , c (AST.Maps Text Text)
    )

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
{-# INLINE makePDeps #-}
makePDeps :: PDeps
makePDeps = MkPDeps
            { _pdepsNormal    = mempty
            , _pdepsImplicit  = mempty
            , _pdepsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'PDeps'.
{-# INLINE pdepsNormal #-}
pdepsNormal :: Lens' PDeps (HashSet FileText)
pdepsNormal = Control.Lens.lens _pdepsNormal
              $ \(MkPDeps {..}) x -> MkPDeps { _pdepsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'PDeps'.
{-# INLINE pdepsImplicit #-}
pdepsImplicit :: Lens' PDeps (HashSet FileText)
pdepsImplicit = Control.Lens.lens _pdepsImplicit
                $ \(MkPDeps {..}) x -> MkPDeps { _pdepsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'PDeps'.
{-# INLINE pdepsOrderOnly #-}
pdepsOrderOnly :: Lens' PDeps (HashSet FileText)
pdepsOrderOnly = Control.Lens.lens _pdepsOrderOnly
                 $ \(MkPDeps {..}) x -> MkPDeps { _pdepsOrderOnly = x, .. }

-- | Converts to @{normal: …, implicit: …, order-only: …}@.
instance ToJSON PDeps where
  toJSON (MkPDeps {..})
    = [ "normal"     .= _pdepsNormal
      , "implicit"   .= _pdepsImplicit
      , "order-only" .= _pdepsOrderOnly
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON PDeps where
  parseJSON = (Aeson.withObject "PDeps" $ \o -> do
                  _pdepsNormal    <- (o .: "normal")     >>= pure
                  _pdepsImplicit  <- (o .: "implicit")   >>= pure
                  _pdepsOrderOnly <- (o .: "order-only") >>= pure
                  pure (MkPDeps {..}))

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, SC.Serial m (HashSet FileText)
         ) => SC.Serial m PDeps

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, SC.CoSerial m (HashSet FileText)
         ) => SC.CoSerial m PDeps

--------------------------------------------------------------------------------
