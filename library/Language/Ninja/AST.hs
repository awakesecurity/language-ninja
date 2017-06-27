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

    -- * @Build@
  , Build, makeBuild
  , buildRule, buildEnv, buildDeps, buildBind
  , BuildConstraint

    -- * @Deps@
  , Deps, makeDeps
  , depsNormal, depsImplicit, depsOrderOnly

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
    , _ninjaSingles   :: !(HashMap FileText Build)
    , _ninjaMultiples :: !(HashMap (HashSet FileText) Build)
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
ninjaSingles :: Lens' Ninja (HashMap FileText Build)
ninjaSingles = Control.Lens.lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens' Ninja (HashMap (HashSet FileText) Build)
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
      fixMultiples :: HashMap (HashSet FileText) Build -> Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> toJSON

      printPair :: HashSet FileText -> Build -> Value
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
      fixMultiples :: Value -> Aeson.Parser (HashMap (HashSet FileText) Build)
      fixMultiples = parseJSON >=> mapM parsePair >=> (HM.fromList .> pure)

      parsePair :: Value -> Aeson.Parser (HashSet FileText, Build)
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
  = ( BuildConstraint c
    , c (HashMap (HashSet FileText) Build)
    , c (HashMap Text (HashSet FileText))
    , c (HashMap Text AST.Rule)
    , c (HashMap FileText Build)
    , c (HashMap Text Int)
    )

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data Build
  = MkBuild
    { _buildRule :: !Text
    , _buildEnv  :: !(AST.Env Text Text)
    , _buildDeps :: !Deps
    , _buildBind :: !(HashMap Text Text)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Build' with all default values.
{-# INLINE makeBuild #-}
makeBuild :: Text
          -- ^ The rule name
          -> AST.Env Text Text
          -- ^ The environment
          -> Build
makeBuild rule env = MkBuild
                     { _buildRule = rule
                     , _buildEnv  = env
                     , _buildDeps = makeDeps
                     , _buildBind = mempty
                     }

-- | A lens into the rule name associated with a 'Build'.
{-# INLINE buildRule #-}
buildRule :: Lens' Build Text
buildRule = lens _buildRule
            $ \(MkBuild {..}) x -> MkBuild { _buildRule = x, .. }

-- | A lens into the environment associated with a 'Build'.
{-# INLINE buildEnv #-}
buildEnv :: Lens' Build (AST.Env Text Text)
buildEnv = lens _buildEnv
           $ \(MkBuild {..}) x -> MkBuild { _buildEnv = x, .. }

-- | A lens into the dependencies associated with a 'Build'.
{-# INLINE buildDeps #-}
buildDeps :: Lens' Build Deps
buildDeps = lens _buildDeps
            $ \(MkBuild {..}) x -> MkBuild { _buildDeps = x, .. }

-- | A lens into the bindings associated with a 'Build'.
{-# INLINE buildBind #-}
buildBind :: Lens' Build (HashMap Text Text)
buildBind = lens _buildBind
            $ \(MkBuild {..}) x -> MkBuild { _buildBind = x, .. }

-- | Converts to @{rule: …, env: …, deps: …, bind: …}@.
instance ToJSON Build where
  toJSON (MkBuild {..})
    = [ "rule" .= _buildRule
      , "env"  .= _buildEnv
      , "deps" .= _buildDeps
      , "bind" .= _buildBind
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Build where
  parseJSON = (Aeson.withObject "Build" $ \o -> do
                  _buildRule <- (o .: "rule") >>= pure
                  _buildEnv  <- (o .: "env")  >>= pure
                  _buildDeps <- (o .: "deps") >>= pure
                  _buildBind <- (o .: "bind") >>= pure
                  pure (MkBuild {..}))

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, BuildConstraint (SC.Serial m)) => SC.Serial m Build

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, BuildConstraint (SC.CoSerial m)) => SC.CoSerial m Build

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Build'.
type BuildConstraint (c :: * -> Constraint)
  = ( c Text
    , c (HashSet FileText)
    , c (HashMap Text Text)
    , c (AST.Maps Text Text)
    )

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
data Deps
  = MkDeps
    { _depsNormal    :: !(HashSet FileText)
    , _depsImplicit  :: !(HashSet FileText)
    , _depsOrderOnly :: !(HashSet FileText)
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Deps' with all default values
{-# INLINE makeDeps #-}
makeDeps :: Deps
makeDeps = MkDeps
            { _depsNormal    = mempty
            , _depsImplicit  = mempty
            , _depsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'Deps'.
{-# INLINE depsNormal #-}
depsNormal :: Lens' Deps (HashSet FileText)
depsNormal = Control.Lens.lens _depsNormal
             $ \(MkDeps {..}) x -> MkDeps { _depsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'Deps'.
{-# INLINE depsImplicit #-}
depsImplicit :: Lens' Deps (HashSet FileText)
depsImplicit = Control.Lens.lens _depsImplicit
               $ \(MkDeps {..}) x -> MkDeps { _depsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'Deps'.
{-# INLINE depsOrderOnly #-}
depsOrderOnly :: Lens' Deps (HashSet FileText)
depsOrderOnly = Control.Lens.lens _depsOrderOnly
                $ \(MkDeps {..}) x -> MkDeps { _depsOrderOnly = x, .. }

-- | Converts to @{normal: …, implicit: …, order-only: …}@.
instance ToJSON Deps where
  toJSON (MkDeps {..})
    = [ "normal"     .= _depsNormal
      , "implicit"   .= _depsImplicit
      , "order-only" .= _depsOrderOnly
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Deps where
  parseJSON = (Aeson.withObject "Deps" $ \o -> do
                  _depsNormal    <- (o .: "normal")     >>= pure
                  _depsImplicit  <- (o .: "implicit")   >>= pure
                  _depsOrderOnly <- (o .: "order-only") >>= pure
                  pure (MkDeps {..}))

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, SC.Serial m (HashSet FileText)
         ) => SC.Serial m Deps

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, SC.CoSerial m (HashSet FileText)
         ) => SC.CoSerial m Deps

--------------------------------------------------------------------------------
