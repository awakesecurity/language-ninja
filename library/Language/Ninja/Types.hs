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
  , PBuild (..)

    -- * @PRule@
  , PRule (..)

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

import qualified Data.ByteString.Char8  as BS
import           Data.Maybe

import           Language.Ninja.Env

import           Flow

-- | FIXME: doc
type Str = BS.ByteString

-- | FIXME: doc
type FileStr = Str

-- | FIXME: doc
data PExpr
  = PExprs [PExpr]
  -- ^ FIXME: doc
  | PLit Str
  -- ^ FIXME: doc
  | PVar Str
  -- ^ FIXME: doc
  deriving (Eq, Show)

-- | FIXME: doc
askExpr :: (MonadIO m) => Env Str Str -> PExpr -> m Str
askExpr e (PExprs xs) = BS.concat <$> mapM (askExpr e) xs
askExpr _ (PLit x)    = pure x
askExpr e (PVar x)    = askVar e x

-- | FIXME: doc
askVar :: (MonadIO m) => Env Str Str -> Str -> m Str
askVar e x = fromMaybe BS.empty <$> askEnv e x

-- | FIXME: doc
addBind :: (MonadIO m) => Env Str Str -> Str -> PExpr -> m ()
addBind e k v = askExpr e v >>= addEnv e k

-- | FIXME: doc
addBinds :: (MonadIO m) => Env Str Str -> [(Str, PExpr)] -> m ()
addBinds e bs = mapM (\(a, b) -> (a,) <$> askExpr e b) bs
                >>= mapM_ (uncurry (addEnv e))

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

-- | FIXME: doc
data PBuild
  = MkPBuild
    { ruleName      :: Str
      -- ^ FIXME: doc
    , env           :: Env Str Str
      -- ^ FIXME: doc
    , depsNormal    :: [FileStr]
      -- ^ FIXME: doc
    , depsImplicit  :: [FileStr]
      -- ^ FIXME: doc
    , depsOrderOnly :: [FileStr]
      -- ^ FIXME: doc
    , buildBind     :: [(Str, Str)]
      -- ^ FIXME: doc
    }
  deriving (Show)

-- | FIXME: doc
newtype PRule
  = MkPRule
    { ruleBind :: [(Str, PExpr)]
      -- ^ FIXME: doc
    }
  deriving (Eq, Show)
