-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Expr.hs
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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Expr
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.AST.Expr
  ( -- * @Expr@
    Expr (..)
  , _Exprs, _Lit, _Var
  , askVar, askExpr, addBind, addBinds
  ) where

import           Control.Arrow             (second)
import           Control.Lens.Prism        (Prism', prism')
import           Data.Foldable             (asum)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (Endo (..))

import           Flow                      ((.>), (|>))

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import qualified Test.QuickCheck           as Q
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen       (Gen (..))
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           Data.Aeson                (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as Aeson

import qualified Language.Ninja.AST.Env    as AST

--------------------------------------------------------------------------------

-- | An expression containing variable references in the Ninja language.
data Expr
  = -- | Sequencing of expressions.
    Exprs [Expr]
  | -- | A literal string.
    Lit Text
  | -- | A variable reference.
    Var Text
  deriving (Eq, Show, Generic)

-- | A prism for the 'PExprs' constructor.
{-# INLINE _Exprs #-}
_Exprs :: Prism' Expr [Expr]
_Exprs = prism' Exprs
         $ \case (Exprs xs) -> Just xs
                 _          -> Nothing

-- | A prism for the 'PLit' constructor.
{-# INLINE _Lit #-}
_Lit :: Prism' Expr Text
_Lit = prism' Lit
       $ \case (Lit t) -> Just t
               _       -> Nothing

-- | A prism for the 'PVar' constructor.
{-# INLINE _Var #-}
_Var :: Prism' Expr Text
_Var = prism' Var
       $ \case (Var t) -> Just t
               _       -> Nothing

-- | Evaluate the given 'PExpr' in the given context (@'Env' 'Text' 'Text'@).
askExpr :: AST.Env Text Text -> Expr -> Text
askExpr e (Exprs xs) = Text.concat (map (askExpr e) xs)
askExpr _ (Lit x)    = x
askExpr e (Var x)    = askVar e x

-- | Look up the given variable in the given context, returning the empty string
--   if the variable was not found.
askVar :: AST.Env Text Text -> Text -> Text
askVar e x = fromMaybe Text.empty (AST.askEnv e x)

-- | Add a binding with the given name ('Text') and value ('PExpr') to the
--   given context.
addBind :: Text -> Expr -> AST.Env Text Text -> AST.Env Text Text
addBind k v e = AST.addEnv k (askExpr e v) e

-- | Add bindings from a list. Note that this function evaluates all the
--   right-hand-sides first, and then adds them all to the environment.
--
--   For example:
--
--   >>> let binds = [("x", PLit "5"), ("y", PVar "x")]
--   >>> AST.headEnv (addBinds binds AST.makeEnv)
--   fromList [("x","5"),("y","")]
addBinds :: [(Text, Expr)] -> AST.Env Text Text -> AST.Env Text Text
addBinds bs e = map (second (askExpr e) .> uncurry AST.addEnv .> Endo) bs
                |> mconcat
                |> (\endo -> appEndo endo e)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Expr

-- | Converts 'Exprs' to a JSON list, 'Lit' to a JSON string,
--   and 'Var' to @{var: …}@.
instance ToJSON Expr where
  toJSON (Exprs xs) = Aeson.toJSON xs
  toJSON (Lit  str) = Aeson.toJSON str
  toJSON (Var  var) = Aeson.object ["var" .= var]

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Expr where
  parseJSON = [ \v -> Exprs <$> Aeson.parseJSON v
              , \v -> Lit   <$> Aeson.parseJSON v
              , Aeson.withObject "PExpr" $ \o -> Var <$> (o .: "var")
              ] |> choice
    where
      choice :: [Aeson.Value -> Aeson.Parser a]
             -> (Aeson.Value -> Aeson.Parser a)
      choice = flip (\v -> map (\f -> f v)) .> fmap asum

-- | Reasonable 'Arbitrary' instance for 'PExpr'.
instance Arbitrary Expr where
  arbitrary = Q.sized go
    where
      go :: Int -> Gen Expr
      go n | n <= 0 = [ Lit <$> Q.resize litLength arbitrary
                      , Var <$> Q.resize varLength arbitrary
                      ] |> Q.oneof
      go n          = [ go 0
                      , do width <- (`mod` maxWidth) <$> arbitrary
                           let subtree = go (n `div` lossRate)
                           Exprs <$> Q.vectorOf width subtree
                      ] |> Q.oneof

      litLength, varLength, lossRate, maxWidth :: Int
      litLength = 10
      varLength = 10
      maxWidth  = 5
      lossRate  = 2

-- | Default 'SC.Serial' instance via 'Generic'.
instance (Monad m, SC.Serial m Text) => SC.Serial m Expr

-- | Default 'SC.CoSerial' instance via 'Generic'.
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Expr

--------------------------------------------------------------------------------
