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
  , AST.Build, AST.makeBuild
  , AST.buildRule, AST.buildEnv, AST.buildDeps, AST.buildBind

    -- * @Deps@
  , AST.Deps, AST.makeDeps
  , AST.depsNormal, AST.depsImplicit, AST.depsOrderOnly

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

import           Control.Arrow            (second)
import           Control.Monad            ((>=>))

import qualified Control.Lens
import           Control.Lens.Lens        (Lens')
import           Control.Lens.Lens
import           Control.Lens.Prism

import           Data.Foldable            (asum)
import qualified Data.Maybe
import           Data.Monoid              (Endo (..))

import qualified Data.ByteString.Char8    as BSC8

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T

import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM

import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HS

import           Data.Hashable            (Hashable)
import           GHC.Generics             (Generic)

import           GHC.Exts                 (Constraint)

import           Data.Aeson
                 (FromJSON (..), KeyValue (..), ToJSON (..), Value, (.:))
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Types         as Aeson

import qualified Test.SmallCheck.Series   as SC

import qualified Language.Ninja.AST.Build as AST
import qualified Language.Ninja.AST.Deps  as AST
import qualified Language.Ninja.AST.Env   as AST
import qualified Language.Ninja.AST.Expr  as AST
import qualified Language.Ninja.AST.Rule  as AST

import           Flow                     ((.>), (|>))

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
    , _ninjaSingles   :: !(HashMap FileText AST.Build)
    , _ninjaMultiples :: !(HashMap (HashSet FileText) AST.Build)
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
ninjaSingles :: Lens' Ninja (HashMap FileText AST.Build)
ninjaSingles = Control.Lens.lens _ninjaSingles
               $ \(MkNinja {..}) x -> MkNinja { _ninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
{-# INLINE ninjaMultiples #-}
ninjaMultiples :: Lens' Ninja (HashMap (HashSet FileText) AST.Build)
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
      fixMultiples :: HashMap (HashSet FileText) AST.Build -> Value
      fixMultiples = HM.toList .> map (uncurry printPair) .> toJSON

      printPair :: HashSet FileText -> AST.Build -> Value
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
      fixMultiples :: Value -> Aeson.Parser (HashMap (HashSet Text) AST.Build)
      fixMultiples = parseJSON >=> mapM parsePair >=> (HM.fromList .> pure)

      parsePair :: Value -> Aeson.Parser (HashSet FileText, AST.Build)
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
  = ( AST.BuildConstraint c
    , c (HashMap (HashSet FileText) AST.Build)
    , c (HashMap Text (HashSet FileText))
    , c (HashMap Text AST.Rule)
    , c (HashMap FileText AST.Build)
    , c (HashMap Text Int)
    )

--------------------------------------------------------------------------------
