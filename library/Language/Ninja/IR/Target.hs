-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/IR/Target.hs
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--   Module      : Language.Ninja.IR.Target
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Types relating to Ninja build targets, outputs, and dependencies.
--
--   @since 0.1.0
module Language.Ninja.IR.Target
  ( -- * @Target@
    Target, makeTarget, targetIText, targetText

    -- * @Output@
  , Output, makeOutput, outputTarget

    -- * @Dependency@
  , Dependency, makeDependency, dependencyTarget, dependencyType
  , DependencyType (..), _NormalDependency, _OrderOnlyDependency
  ) where

import qualified Control.Lens              as Lens

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson                as Aeson

import           Control.DeepSeq           (NFData)
import           Data.Hashable             (Hashable)
import           Data.String               (IsString)
import           GHC.Generics              (Generic)

import           Test.SmallCheck.Series    ((>>-))
import qualified Test.SmallCheck.Series    as SC

import qualified Language.Ninja.Misc.IText as Misc

import           Flow                      ((.>), (|>))

--------------------------------------------------------------------------------

-- | This type represents a Ninja target name.
--
--   @since 0.1.0
newtype Target
  = MkTarget
    { _targetIText :: Misc.IText
    }
  deriving ( Eq, Ord, Show, Read, IsString, Generic, Hashable, NFData
           , Aeson.ToJSON, Aeson.FromJSON, Aeson.ToJSONKey, Aeson.FromJSONKey )

-- | Construct a 'Target' from some 'Text'.
--
--   @since 0.1.0
{-# INLINE makeTarget #-}
makeTarget :: Text -> Target
makeTarget = Lens.view Misc.itext .> MkTarget

-- | An isomorphism between a 'Target' and its underlying 'IText'.
--
--   @since 0.1.0
{-# INLINE targetIText #-}
targetIText :: Lens.Iso' Target Misc.IText
targetIText = Lens.iso _targetIText MkTarget

-- | An isomorphism that gives access to a 'Text'-typed view of a 'Target',
--   even though the underlying data has type 'IText'.
--
--   This is equivalent to @targetIText . from itext@.
--
--   @since 0.1.0
{-# INLINE targetText #-}
targetText :: Lens.Iso' Target Text
targetText = targetIText . Lens.from Misc.itext

-- | Uses the underlying 'IText' instance.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Target where
  series = SC.newtypeCons MkTarget

-- | Uses the underlying 'IText' instance.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Target where
  coseries rs = SC.newtypeAlts rs
                >>- \f -> pure (_targetIText .> f)

--------------------------------------------------------------------------------

-- | A Ninja build output.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_outputs here>.
--
--   @since 0.1.0
newtype Output
  = MkOutput
    { _outputTarget :: Target
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct an 'Output'.
--
--   @since 0.1.0
{-# INLINE makeOutput #-}
makeOutput :: Target
           -- ^ The underlying target.
           -> Output
makeOutput = MkOutput

-- | A lens for the 'Target' of an 'Output'.
--
--   @since 0.1.0
{-# INLINE outputTarget #-}
outputTarget :: Lens.Lens' Output Target
outputTarget = Lens.lens _outputTarget
               $ \(MkOutput {..}) new -> MkOutput { _outputTarget = new, .. }

-- | Converts to @{target: …, type: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Output where
  toJSON (MkOutput {..})
    = [ "target" .= _outputTarget
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Output where
  parseJSON = (Aeson.withObject "Output" $ \o -> do
                  _outputTarget <- (o .: "target") >>= pure
                  pure (MkOutput {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable Output

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Output

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Output

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Output

--------------------------------------------------------------------------------

-- | A build dependency.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_dependencies here>.
--
--   @since 0.1.0
data Dependency
  = MkDependency
    { _dependencyTarget :: !Target
    , _dependencyType   :: !DependencyType
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct a 'Dependency'.
--
--   @since 0.1.0
{-# INLINE makeDependency #-}
makeDependency :: Target
               -- ^ The underlying target.
               -> DependencyType
               -- ^ The dependency type (normal, implicit, or order-only).
               -> Dependency
makeDependency = MkDependency

-- | A lens for the 'Target' of a 'Dependency'.
--
--   @since 0.1.0
{-# INLINE dependencyTarget #-}
dependencyTarget :: Lens.Lens' Dependency Target
dependencyTarget
  = Lens.lens _dependencyTarget
    $ \(MkDependency {..}) new -> MkDependency { _dependencyTarget = new, .. }

-- | A lens for the 'DependencyType' of a 'Dependency'.
--
--   @since 0.1.0
{-# INLINE dependencyType #-}
dependencyType :: Lens.Lens' Dependency DependencyType
dependencyType
  = Lens.lens _dependencyType
    $ \(MkDependency {..}) new -> MkDependency { _dependencyType = new, .. }

-- | Converts to @{target: …, type: …}@.
--
--   @since 0.1.0
instance Aeson.ToJSON Dependency where
  toJSON (MkDependency {..})
    = [ "target" .= _dependencyTarget
      , "type"   .= _dependencyType
      ] |> Aeson.object

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON Dependency where
  parseJSON = (Aeson.withObject "Dependency" $ \o -> do
                  _dependencyTarget <- (o .: "target") >>= pure
                  _dependencyType   <- (o .: "type")   >>= pure
                  pure (MkDependency {..}))

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable Dependency

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData Dependency

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.Serial m Text) => SC.Serial m Dependency

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m, SC.CoSerial m Text) => SC.CoSerial m Dependency

--------------------------------------------------------------------------------

-- | The type of a 'Dependency': normal, implicit, or order-only.
--
--   @since 0.1.0
data DependencyType
  = -- | A normal dependency. These are listed in the @$in@ variable and changes
    --   in the relevant target result in a rule execution.
    --
    --   @since 0.1.0
    NormalDependency
  | -- | An order-only dependency. These are listed in the @$in@ variable, but
    --   are only rebuilt if there is at least one non-order-only dependency
    --   that is out of date.
    --
    --   FIXME: double check this interpretation of the Ninja manual
    --
    --   @since 0.1.0
    OrderOnlyDependency
  deriving (Eq, Ord, Show, Read, Generic)

-- | A prism for the 'NormalDependency' constructor.
--
--   @since 0.1.0
{-# INLINE _NormalDependency #-}
_NormalDependency :: Lens.Prism' DependencyType ()
_NormalDependency = Lens.prism' (const NormalDependency)
                    $ \case NormalDependency -> Just ()
                            _                -> Nothing

-- | A prism for the 'OrderOnlyDependency' constructor.
--
--   @since 0.1.0
{-# INLINE _OrderOnlyDependency #-}
_OrderOnlyDependency :: Lens.Prism' DependencyType ()
_OrderOnlyDependency = Lens.prism' (const OrderOnlyDependency)
                       $ \case OrderOnlyDependency -> Just ()
                               _                   -> Nothing

-- | Converts to @"normal"@, @"implicit"@, and @"order-only"@ respectively.
--
--   @since 0.1.0
instance Aeson.ToJSON DependencyType where
  toJSON NormalDependency    = "normal"
  toJSON OrderOnlyDependency = "order-only"

-- | Inverse of the 'Aeson.ToJSON' instance.
--
--   @since 0.1.0
instance Aeson.FromJSON DependencyType where
  parseJSON = (Aeson.withText "DependencyType" $ \case
                  "normal"     -> pure NormalDependency
                  "order-only" -> pure OrderOnlyDependency
                  owise        -> [ "Invalid dependency type "
                                  , "\"", owise, "\"; should be one of "
                                  , "[\"normal\", \"order-only\"]"
                                  ] |> mconcat |> Text.unpack |> fail)

-- | Default 'Hashable' instance via 'Generic'.
--
--   @since 0.1.0
instance Hashable DependencyType

-- | Default 'NFData' instance via 'Generic'.
--
--   @since 0.1.0
instance NFData DependencyType

-- | Default 'SC.Serial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m) => SC.Serial m DependencyType

-- | Default 'SC.CoSerial' instance via 'Generic'.
--
--   @since 0.1.0
instance (Monad m) => SC.CoSerial m DependencyType

--------------------------------------------------------------------------------
