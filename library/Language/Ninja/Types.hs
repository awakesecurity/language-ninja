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
askExpr :: (MonadIO m) => Env Str Str -> PExpr -> m Str
askExpr e (PExprs xs) = BSC8.concat <$> mapM (askExpr e) xs
askExpr _ (PLit x)    = pure x
askExpr e (PVar x)    = askVar e x

-- | FIXME: doc
askVar :: (MonadIO m) => Env Str Str -> Str -> m Str
askVar e x = fromMaybe BSC8.empty <$> askEnv e x

-- | FIXME: doc
addBind :: (MonadIO m) => Env Str Str -> Str -> PExpr -> m ()
addBind e k v = askExpr e v >>= addEnv e k

-- | FIXME: doc
addBinds :: (MonadIO m) => Env Str Str -> [(Str, PExpr)] -> m ()
addBinds e bs = mapM (\(a, b) -> (a,) <$> askExpr e b) bs
                >>= mapM_ (uncurry (addEnv e))

--------------------------------------------------------------------------------

-- | A parsed Ninja file.
data PNinja
  = MkPNinja
    { _pninjaRules     :: [(Str, PRule)]
    , _pninjaSingles   :: [(FileStr, PBuild)]
    , _pninjaMultiples :: [([FileStr], PBuild)]
    , _pninjaPhonys    :: [(Str, [FileStr])]
    , _pninjaDefaults  :: [FileStr]
    , _pninjaPools     :: [(Str, Int)]
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
pninjaRules :: Lens' PNinja [(Str, PRule)]
pninjaRules = lens _pninjaRules
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaRules = x, .. }

-- | The set of build declarations with precisely one output.
pninjaSingles :: Lens' PNinja [(FileStr, PBuild)]
pninjaSingles = lens _pninjaSingles
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaSingles = x, .. }

-- | The set of build declarations with two or more outputs.
pninjaMultiples :: Lens' PNinja [([FileStr], PBuild)]
pninjaMultiples = lens _pninjaMultiples
                  $ \(MkPNinja {..}) x -> MkPNinja { _pninjaMultiples = x, .. }

-- | The set of phony build declarations.
pninjaPhonys :: Lens' PNinja [(Str, [FileStr])]
pninjaPhonys = lens _pninjaPhonys
               $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPhonys = x, .. }

-- | The set of default targets.
pninjaDefaults :: Lens' PNinja [FileStr]
pninjaDefaults = lens _pninjaDefaults
                 $ \(MkPNinja {..}) x -> MkPNinja { _pninjaDefaults = x, .. }

-- | A mapping from pool names to pool depth integers.
pninjaPools :: Lens' PNinja [(Str, Int)]
pninjaPools = lens _pninjaPools
              $ \(MkPNinja {..}) x -> MkPNinja { _pninjaPools = x, .. }

--------------------------------------------------------------------------------

-- | A parsed Ninja @build@ declaration.
data PBuild
  = MkPBuild
    { _pbuildRule :: !Str
    , _pbuildEnv  :: !(Env Str Str)
    , _pbuildDeps :: !PDeps
    , _pbuildBind :: ![(Str, Str)]
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
pbuildBind :: Lens' PBuild [(Str, Str)]
pbuildBind = lens _pbuildBind
             $ \(MkPBuild {..}) x -> MkPBuild { _pbuildBind = x, .. }

--------------------------------------------------------------------------------

-- | A set of Ninja build dependencies.
data PDeps
  = MkPDeps
    { _pdepsNormal    :: ![FileStr]
    , _pdepsImplicit  :: ![FileStr]
    , _pdepsOrderOnly :: ![FileStr]
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
pdepsNormal :: Lens' PDeps [FileStr]
pdepsNormal = lens _pdepsNormal
              $ \(MkPDeps {..}) x -> MkPDeps { _pdepsNormal = x, .. }

-- | A lens into the set of implicit dependencies in a 'PDeps'.
pdepsImplicit :: Lens' PDeps [FileStr]
pdepsImplicit = lens _pdepsImplicit
                $ \(MkPDeps {..}) x -> MkPDeps { _pdepsImplicit = x, .. }

-- | A lens into the set of order-only dependencies in a 'PDeps'.
pdepsOrderOnly :: Lens' PDeps [FileStr]
pdepsOrderOnly = lens _pdepsOrderOnly
                 $ \(MkPDeps {..}) x -> MkPDeps { _pdepsOrderOnly = x, .. }

--------------------------------------------------------------------------------

-- | A parsed Ninja @rule@ declaration.
newtype PRule
  = MkPRule
    { _pruleBind :: [(Str, PExpr)]
    }
  deriving (Eq, Show)

-- | Construct a 'PRule' with all default values
makePRule :: PRule
makePRule = MkPRule
            { _pruleBind = mempty
            }

-- | The set of bindings in scope during the execution of this rule.
pruleBind :: Lens' PRule [(Str, PExpr)]
pruleBind = lens _pruleBind (const MkPRule)

--------------------------------------------------------------------------------
