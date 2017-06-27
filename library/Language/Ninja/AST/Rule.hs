-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Rule.hs
--
-- License:
--     Copyright 2017 Awake Security
--
--     Licensed under the Apache License, Version 2.0 (the "License");
--     you may not use this file except in compliance with the License.
--     You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--     Unless required by applicable law or agreed to in writing, software
--     distributed under the License is distributed on an "AS IS" BASIS,
--     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--     See the License for the specific language governing permissions and
--     limitations under the License.

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Rule
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.AST.Rule
  ( module Language.Ninja.AST.Rule -- FIXME: specific export list
  ) where

import           Control.Lens.Lens       (Lens', lens)

import           Flow                    ((.>), (|>))

import           Data.HashMap.Strict     (HashMap)
import           Data.Text               (Text)

import           Data.Hashable           (Hashable)
import           GHC.Generics            (Generic)

import qualified Test.SmallCheck.Series  as SC

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as Aeson

import qualified Language.Ninja.AST.Expr as AST

--------------------------------------------------------------------------------

-- | A parsed Ninja @rule@ declaration.
newtype Rule
  = MkRule
    { _ruleBind :: HashMap Text AST.Expr
    }
  deriving (Eq, Show, Generic)

-- | Construct a 'Rule' with all default values
{-# INLINE makeRule #-}
makeRule :: Rule
makeRule = MkRule
           { _ruleBind = mempty
           }

-- | The set of bindings in scope during the execution of this rule.
{-# INLINE ruleBind #-}
ruleBind :: Lens' Rule (HashMap Text AST.Expr)
ruleBind = lens _ruleBind (const MkRule)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Rule

-- | Uses the 'ToJSON' instance of the underlying @'HashMap' 'Text' 'PEXpr'@.
instance ToJSON Rule where
  toJSON = _ruleBind .> Aeson.toJSON

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Rule where
  parseJSON = Aeson.parseJSON .> fmap MkRule

-- | Default 'SC.Serial' instance via 'Generic'.
instance ( Monad m, SC.Serial m (HashMap Text AST.Expr)
         ) => SC.Serial m Rule

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance ( Monad m, SC.CoSerial m (HashMap Text AST.Expr)
         ) => SC.CoSerial m Rule

--------------------------------------------------------------------------------
