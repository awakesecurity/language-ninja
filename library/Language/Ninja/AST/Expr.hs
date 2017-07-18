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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Language.Ninja.AST.Expr
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   This module contains a type representing a string that potentially contains
--   variable references in the parsed Ninja AST, along with any supporting or
--   related types.
--
--   @since 0.1.0
module Language.Ninja.AST.Expr
  ( -- * @Expr@
    Expr (..)
  , _Exprs, _Lit, _Var
  , askVar, askExpr, addBind, addBinds
  , normalizeExpr
  , ExprConstraint
  ) where

import           Control.Arrow             (second)

import qualified Control.Lens              as Lens

import           Data.Foldable             (asum)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (Endo (Endo, appEndo), (<>))

import           Flow                      ((.>), (|>))

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Control.DeepSeq           (NFData)
import           Data.Data                 (Data)
import           Data.Hashable             (Hashable)
import           GHC.Generics              (Generic)

import qualified Test.QuickCheck           as QC
import           Test.QuickCheck.Instances ()

import qualified Test.SmallCheck.Series    as SC

import           GHC.Exts                  (Constraint)

import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson                as Aeson

import qualified Language.Ninja.AST.Env    as AST
import qualified Language.Ninja.Misc       as Misc

--------------------------------------------------------------------------------

-- | An expression containing variable references in the Ninja language.
--
--   @since 0.1.0
data Expr ann
  = -- | Sequencing of expressions.
    --
    --   @since 0.1.0
    Exprs !ann ![Expr ann]
  | -- | A literal string.
    --
    --   @since 0.1.0
    Lit   !ann !Text
  | -- | A variable reference.
    --
    --   @since 0.1.0
    Var   !ann !Text
  deriving (Eq, Show, Generic, Data, Functor, Foldable, Traversable)

-- | A prism for the 'Exprs' constructor.
--
--   @since 0.1.0
{-# INLINE _Exprs #-}
_Exprs :: Lens.Prism' (Expr ann) (ann, [Expr ann])
_Exprs = Lens.prism' (uncurry Exprs)
         $ \case (Exprs ann es) -> Just (ann, es)
                 _              -> Nothing

-- | A prism for the 'Lit' constructor.
--
--   @since 0.1.0
{-# INLINE _Lit #-}
_Lit :: Lens.Prism' (Expr ann) (ann, Text)
_Lit = Lens.prism' (uncurry Lit)
       $ \case (Lit ann text) -> Just (ann, text)
               _              -> Nothing

-- | A prism for the 'Var' constructor.
--
--   @since 0.1.0
{-# INLINE _Var #-}
_Var :: Lens.Prism' (Expr ann) (ann, Text)
_Var = Lens.prism' (uncurry Var)
       $ \case (Var ann name) -> Just (ann, name)
               _              -> Nothing

-- | Evaluate the given 'Expr' in the given context (@'Env' 'Text' 'Text'@).
--
--   @since 0.1.0
askExpr :: AST.Env Text Text -> Expr ann -> Text
askExpr e (Exprs _ xs) = Text.concat (map (askExpr e) xs)
askExpr _ (Lit   _ x)  = x
askExpr e (Var   _ x)  = askVar e x

-- | Look up the given variable in the given context, returning the empty string
--   if the variable was not found.
--
--   @since 0.1.0
askVar :: AST.Env Text Text -> Text -> Text
askVar e x = fromMaybe Text.empty (AST.askEnv e x)

-- | Add a binding with the given name ('Text') and value ('Expr') to the
--   given context.
--
--   @since 0.1.0
addBind :: Text -> Expr ann -> AST.Env Text Text -> AST.Env Text Text
addBind k v e = AST.addEnv k (askExpr e v) e

-- | Add bindings from a list. Note that this function evaluates all the
--   right-hand-sides first, and then adds them all to the environment.
--
--   For example:
--
--   >>> :set -XOverloadedStrings
--   >>> let binds = [("x", Lit () "5"), ("y", Var () "x")]
--   >>> AST.headEnv (addBinds binds AST.makeEnv)
--   fromList [("x","5"),("y","")]
--
--   @since 0.1.0
addBinds :: [(Text, Expr ann)] -> AST.Env Text Text -> AST.Env Text Text
addBinds bs e = map (second (askExpr e) .> uncurry AST.addEnv .> Endo) bs
                |> mconcat
                |> (\endo -> appEndo endo e)

-- | Normalize an 'Expr' by recursively flattening any 'Exprs' nodes, removing
--   empty 'Lit' nodes, combining adjacent 'Lit' nodes, and pulling out the
--   interior of the top-level 'Exprs' node if it has only one subnode.
--
--   The number of 'Exprs' nodes in the output is guaranteed to be 0 or 1.
--
--   If it is 0, then there is exactly one node of any type in the output.
--
--   The output is thus isomorphic to @(Maybe ann, [(ann, Either Text Text)])@,
--   where the @Maybe ann@ represents the annotation of the top-level 'Exprs'
--   node if it exists.
--
--   @since 0.1.0
normalizeExpr :: forall ann. (Monoid ann) => Expr ann -> Expr ann
normalizeExpr = flatten .> removeEmpty .> combineAdj .> listToExpr
  where
    flatten :: Expr ann -> [Expr ann]
    flatten (Exprs _ es) = concatMap flatten es
    flatten owise        = [owise]

    removeEmpty :: [Expr ann] -> [Expr ann]
    removeEmpty []                = []
    removeEmpty (Lit _ "" : rest) = removeEmpty rest
    removeEmpty (owise    : rest) = owise : removeEmpty rest

    combineAdj :: [Expr ann] -> [Expr ann]
    combineAdj = (\case
      []                               -> []
      (Lit annX x : Lit annY y : rest) -> (Lit (annX <> annY) (x <> y))
                                          |> (\e -> combineAdj (e : rest))
      (owise                   : rest) -> owise : combineAdj rest)

    listToExpr :: [Expr ann] -> Expr ann
    listToExpr [e] = e
    listToExpr es  = Exprs (mconcat (map (Lens.view Misc.annotation) es)) es

-- | The usual definition for 'Lens.Plated'.
--
--   @since 0.1.0
instance (Data ann) => Lens.Plated (Expr ann)

-- | The usual definition for 'Misc.Annotated'.
--
--   @since 0.1.0
instance Misc.Annotated Expr where
  annotation' f = Lens.lens (helper .> fst) (helper .> snd)
    where
      helper (Exprs ann   es) = (ann, \x -> Exprs x (map (fmap f) es))
      helper (Lit   ann text) = (ann, \x -> Lit   x text)
      helper (Var   ann name) = (ann, \x -> Var   x name)

-- | Converts 'Exprs' to @{ann: …, exprs: […]}@, 'Lit' to @{ann: …, lit: […]}@,
--   and 'Var' to @{ann: …, var: …}@.
--
--   @since 0.1.0
instance (Aeson.ToJSON ann) => Aeson.ToJSON (Expr ann) where
  toJSON (Exprs ann   es) = Aeson.object ["ann" .= ann, "exprs" .= es]
  toJSON (Lit   ann text) = Aeson.object ["ann" .= ann, "lit"   .= text]
  toJSON (Var   ann name) = Aeson.object ["ann" .= ann, "var"   .= name]

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance (Aeson.FromJSON ann) => Aeson.FromJSON (Expr ann) where
  parseJSON = Aeson.withObject "Expr" $ \o -> do
    ann <- o .: "ann"
    asum [ Exprs ann <$> (o .: "exprs")
         , Lit   ann <$> (o .: "lit")
         , Var   ann <$> (o .: "var")
         ]

-- | Reasonable 'QC.Arbitrary' instance for 'Expr'.
--
--   @since 0.1.0
instance forall ann. (QC.Arbitrary ann) => QC.Arbitrary (Expr ann) where
  arbitrary = QC.sized go
    where
      go :: Int -> QC.Gen (Expr ann)
      go n | n <= 0 = [ litG (QC.resize litLength QC.arbitrary)
                      , varG (QC.resize varLength QC.arbitrary)
                      ] |> QC.oneof
      go n          = [ go 0
                      , do width <- (`mod` maxWidth) <$> QC.arbitrary
                           let subtree = go (n `div` lossRate)
                           Exprs <$> QC.arbitrary <*> QC.vectorOf width subtree
                      ] |> QC.oneof

      litG, varG :: QC.Gen Text -> QC.Gen (Expr ann)
      litG g = Lit <$> QC.arbitrary <*> g
      varG g = Var <$> QC.arbitrary <*> g

      litLength, varLength, lossRate, maxWidth :: Int
      litLength = 10
      varLength = 10
      maxWidth  = 5
      lossRate  = 2

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance (Hashable ann) => Hashable (Expr ann)

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance (NFData ann) => NFData (Expr ann)

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, ExprConstraint (SC.Serial m) ann
         ) => SC.Serial m (Expr ann)

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance ( Monad m, ExprConstraint (SC.CoSerial m) ann
         ) => SC.CoSerial m (Expr ann)

-- | The set of constraints required for a given constraint to be automatically
--   computed for a 'Expr'.
--
--   @since 0.1.0
type ExprConstraint (c :: * -> Constraint) (ann :: *) = (c Text, c ann)

--------------------------------------------------------------------------------
