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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- |
--   Module      : Language.Ninja.Types
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakenetworks.com
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
  , Env, newEnv, addEnv

    -- * Miscellaneous
  , Str, FileStr
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.IO.Class

import           Control.Lens.Lens

import qualified Data.ByteString.Char8  as BSC8
import           Data.Maybe

import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM

import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HS

import           Data.Monoid

import           Language.Ninja.Env

import           Flow

--------------------------------------------------------------------------------

-- | FIXME: doc
type Str = BSC8.ByteString

-- | FIXME: doc
type FileStr = Str

--------------------------------------------------------------------------------

-- | FIXME: doc
data PExpr
  = -- | FIXME: doc
    PExprs [PExpr]
  | -- | FIXME: doc
    PLit Str
  | -- | FIXME: doc
    PVar Str
  deriving (Eq, Show)

-- | FIXME: doc
askExpr :: Env Str Str -> PExpr -> Str
askExpr e (PExprs xs) = BSC8.concat (map (askExpr e) xs)
askExpr _ (PLit x)    = x
askExpr e (PVar x)    = askVar e x

-- | FIXME: doc
askVar :: Env Str Str -> Str -> Str
askVar e x = fromMaybe BSC8.empty (askEnv e x)

-- | FIXME: doc
addBind :: Str -> PExpr -> Env Str Str -> Env Str Str
addBind k v e = addEnv k (askExpr e v) e

-- | FIXME: doc
addBinds :: [(Str, PExpr)] -> Env Str Str -> Env Str Str
addBinds bs e = map (second (askExpr e) .> uncurry addEnv .> Endo) bs
                |> mconcat
                |> (\endo -> appEndo endo e)

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data PNinja
  = MkPNinja
    { _pninjaRules     :: HashMap Str PRule
    , _pninjaSingles   :: HashMap FileStr PBuild
    , _pninjaMultiples :: HashMap (HashSet FileStr) PBuild
    , _pninjaPhonys    :: HashMap Str (HashSet FileStr)
    , _pninjaDefaults  :: HashSet FileStr
    , _pninjaPools     :: HashMap Str Int
    }
  deriving (Show)

-- | Construct a 'PNinja' with all default values
makePNinja :: PNinja
makePNinja = MkPNinja
             { _pninjaRules     = mempty
             , _pninjaSingles   = mempty
             , _pninjaMultiples = mempty
             , _pninjaPhonys    = mempty
             , _pninjaDefaults  = mempty
             , _pninjaPools     = mempty
             }

-- | The rules defined in a parsed Ninja file.
pninjaRules :: Lens' PNinja (HashMap Str PRule)
pninjaRules = lens _pninjaRules
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
pninjaSingles :: Lens' PNinja (HashMap FileStr PBuild)
pninjaSingles = lens _pninjaSingles
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
pninjaMultiples :: Lens' PNinja (HashMap (HashSet FileStr) PBuild)
pninjaMultiples = lens _pninjaMultiples
                  $ \(MkPNinja {..}) x -> MkPNinja { _pninjaMultiples = x, .. }

-- | The set of phony build declarations.
pninjaPhonys :: Lens' PNinja (HashMap Str (HashSet FileStr))
pninjaPhonys = lens _pninjaPhonys
               $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPhonys = x, .. }

-- | The set of default targets.
pninjaDefaults :: Lens' PNinja (HashSet FileStr)
pninjaDefaults = lens _pninjaDefaults
                 $ \(MkPNinja {..}) x -> MkPNinja { _pninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
pninjaPools :: Lens' PNinja (HashMap Str Int)
pninjaPools = lens _pninjaPools
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPools = x, .. }

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data PBuild
  = MkPBuild
    { _pbuildRule :: !Str
    , _pbuildEnv  :: !(Env Str Str)
    , _pbuildDeps :: !PDeps
    , _pbuildBind :: !(HashMap Str Str)
    }
  deriving (Show)

-- | Construct a 'PBuild' with all default values.
makePBuild :: Str
           -- ^ The rule name
           -> Env Str Str
           -- ^ The environment
           -> PBuild
makePBuild rule env = MkPBuild
                      { _pbuildRule = rule
                      , _pbuildEnv  = env
                      , _pbuildDeps = makePDeps
                      , _pbuildBind = mempty
                      }

-- | A lens into the rule name associated with a 'PBuild'.
pbuildRule :: Lens' PBuild Str
pbuildRule = lens _pbuildRule
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildRule = x, .. }

-- | A lens into the environment associated with a 'PBuild'.
pbuildEnv :: Lens' PBuild (Env Str Str)
pbuildEnv = lens _pbuildEnv
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildEnv = x, .. }

-- | A lens into the dependencies associated with a 'PBuild'.
pbuildDeps :: Lens' PBuild PDeps
pbuildDeps = lens _pbuildDeps
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildDeps = x, .. }

-- | A lens into the bindings associated with a 'PBuild'.
pbuildBind :: Lens' PBuild (HashMap Str Str)
pbuildBind = lens _pbuildBind
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildBind = x, .. }

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
data PDeps
  = MkPDeps
    { _pdepsNormal    :: !(HashSet FileStr)
    , _pdepsImplicit  :: !(HashSet FileStr)
    , _pdepsOrderOnly :: !(HashSet FileStr)
    }
  deriving (Eq, Show)

-- | Construct a 'PDeps' with all default values
makePDeps :: PDeps
makePDeps = MkPDeps
            { _pdepsNormal    = mempty
            , _pdepsImplicit  = mempty
            , _pdepsOrderOnly = mempty
            }

-- | A lens into the set of normal dependencies in a 'PDeps'.
pdepsNormal :: Lens' PDeps (HashSet FileStr)
pdepsNormal = lens _pdepsNormal
              $ \(MkPDeps {..}) x -> MkPDeps { _pdepsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'PDeps'.
pdepsImplicit :: Lens' PDeps (HashSet FileStr)
pdepsImplicit = lens _pdepsImplicit
                $ \(MkPDeps {..}) x -> MkPDeps { _pdepsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'PDeps'.
pdepsOrderOnly :: Lens' PDeps (HashSet FileStr)
pdepsOrderOnly = lens _pdepsOrderOnly
                 $ \(MkPDeps {..}) x -> MkPDeps { _pdepsOrderOnly = x, .. }

--------------------------------------------------------------------------------

-- | A parsed Ninja @rule@ declaration.
newtype PRule
  = MkPRule
    { _pruleBind :: HashMap Str PExpr
    }
  deriving (Eq, Show)

-- | Construct a 'PRule' with all default values
makePRule :: PRule
makePRule = MkPRule
            { _pruleBind = mempty
            }

-- | The set of bindings in scope during the execution of this rule.
pruleBind :: Lens' PRule (HashMap Str PExpr)
pruleBind = lens _pruleBind (const MkPRule)

--------------------------------------------------------------------------------
