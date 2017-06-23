-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/AST/Target.hs
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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
--   Module      : Language.Ninja.AST.Target
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Types relating to Ninja build targets, outputs, and dependencies.
module Language.Ninja.AST.Target
  ( -- * @Target@
    Target, makeTarget, targetIText, targetText
    -- * @Output@
  , Output, makeOutput, outputTarget, outputType
  , OutputType (..)
    -- * @Dependency@
  , Dependency, makeDependency, dependencyTarget, dependencyType
  , DependencyType (..)
  ) where

import           Language.Ninja.Misc.IText

import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Data.Aeson                (FromJSON(..), FromJSONKey,
                                            KeyValue(..), ToJSON(..), ToJSONKey,
                                            (.:))
import qualified Data.Aeson                as Aeson

import           Data.Hashable             (Hashable (..))
import           Data.String               (IsString (..))
import           GHC.Generics              (Generic)

import           Control.Lens.Getter       (view)
import           Control.Lens.Iso          (Iso')
import           Control.Lens.Lens         (Lens')
import qualified Control.Lens

import           Flow                      ((|>), (.>))

--------------------------------------------------------------------------------

-- | This type represents a Ninja target name.
newtype Target
  = MkTarget
    { _targetIText :: IText
    }
  deriving ( Eq, Ord, Show, Read, IsString, Generic, Hashable
           , ToJSON, FromJSON, ToJSONKey, FromJSONKey )

-- | Construct a 'Target' from some 'Text'.
makeTarget :: Text -> Target
makeTarget = view itext .> MkTarget

-- | An isomorphism between a 'Target' and its underlying 'IText'.
targetIText :: Iso' Target IText
targetIText = Control.Lens.iso _targetIText MkTarget

-- | An isomorphism that gives access to a 'Text'-typed view of a 'Target',
--   even though the underlying data has type 'IText'.
--
--   This is equivalent to @targetIText . Control.Lens.from itext@.
targetText :: Iso' Target Text
targetText = targetIText . Control.Lens.from itext

--------------------------------------------------------------------------------

-- | A Ninja build output.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_outputs here>.
data Output
  = MkOutput
    { _outputTarget :: !Target
    , _outputType   :: !OutputType
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct an 'Output'.
makeOutput :: Target
           -- ^ The underlying target.
           -> OutputType
           -- ^ The output type (explicit or implicit).
           -> Output
makeOutput = MkOutput

-- | A lens for the 'Target' of an 'Output'.
outputTarget :: Lens' Output Target
outputTarget = Control.Lens.lens _outputTarget
               $ \(MkOutput {..}) new -> MkOutput { _outputTarget = new, .. }

-- | A lens for the 'OutputType' of an 'Output'.
outputType :: Lens' Output OutputType
outputType = Control.Lens.lens _outputType
             $ \(MkOutput {..}) new -> MkOutput { _outputType = new, .. }

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Output

-- | Converts to @{target: …, type: …}@.
instance ToJSON Output where
  toJSON (MkOutput {..})
    = [ "target" .= _outputTarget
      , "type"   .= _outputType
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Output where
  parseJSON = (Aeson.withObject "Output" $ \o -> do
                  _outputTarget <- (o .: "target") >>= pure
                  _outputType   <- (o .: "type")   >>= pure
                  pure (MkOutput {..}))

--------------------------------------------------------------------------------

-- | The type of an 'Output': explicit or implicit.
data OutputType
  = -- | Explicit outputs are listed in the @$out@ variable.
    ExplicitOutput
  | -- | Implicit outputs are _not_ listed in the @$out@ variable.
    ImplicitOutput
  deriving (Eq, Ord, Show, Read, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable OutputType

-- | Converts to @"explicit"@ and @"implicit"@ respectively.
instance ToJSON OutputType where
  toJSON ExplicitOutput = "explicit"
  toJSON ImplicitOutput = "implicit"

-- | Inverse of the 'ToJSON' instance.
instance FromJSON OutputType where
  parseJSON = (Aeson.withText "OutputType" $ \case
                  "explicit" -> pure ExplicitOutput
                  "implicit" -> pure ImplicitOutput
                  owise      -> [ "Invalid output type "
                                , "\"", owise, "\"; should be one of "
                                , "[\"explict\", \"implicit\"]"
                                ] |> mconcat |> T.unpack |> fail)

--------------------------------------------------------------------------------

-- | A build dependency.
--
--   More information is available
--   <https://ninja-build.org/manual.html#ref_dependencies here>.
data Dependency
  = MkDependency
    { _dependencyTarget :: !Target
    , _dependencyType   :: !DependencyType
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Construct a 'Dependency'.
makeDependency :: Target
               -- ^ The underlying target.
               -> DependencyType
               -- ^ The dependency type (normal, implicit, or order-only).
               -> Dependency
makeDependency = MkDependency

-- | A lens for the 'Target' of a 'Dependency'.
dependencyTarget :: Lens' Dependency Target
dependencyTarget
  = Control.Lens.lens _dependencyTarget
    $ \(MkDependency {..}) new -> MkDependency { _dependencyTarget = new, .. }

-- | A lens for the 'DependencyType' of a 'Dependency'.
dependencyType :: Lens' Dependency DependencyType
dependencyType
  = Control.Lens.lens _dependencyType
    $ \(MkDependency {..}) new -> MkDependency { _dependencyType = new, .. }

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable Dependency

-- | Converts to @{target: …, type: …}@.
instance ToJSON Dependency where
  toJSON (MkDependency {..})
    = [ "target" .= _dependencyTarget
      , "type"   .= _dependencyType
      ] |> Aeson.object

-- | Inverse of the 'ToJSON' instance.
instance FromJSON Dependency where
  parseJSON = (Aeson.withObject "Dependency" $ \o -> do
                  _dependencyTarget <- (o .: "target") >>= pure
                  _dependencyType   <- (o .: "type")   >>= pure
                  pure (MkDependency {..}))

--------------------------------------------------------------------------------

-- | The type of a 'Dependency': normal, implicit, or order-only.
data DependencyType
  = -- | A normal dependency. These are listed in the @$in@ variable and changes
    --   in the relevant target result in a rule execution.
    NormalDependency
  | -- | An implicit dependency. These have the same semantics as normal
    --   dependencies, except they do not show up in the @$in@ variable.
    ImplicitDependency
  | -- | An order-only dependency. These are listed in the @$in@ variable, but
    --   are only rebuilt if there is at least one non-order-only dependency
    --   that is out of date.
    --
    --   FIXME: double check this interpretation of the Ninja manual
    OrderOnlyDependency
  deriving (Eq, Ord, Show, Read, Generic)

-- | Default 'Hashable' instance via 'Generic'.
instance Hashable DependencyType

-- | Converts to @"normal"@, @"implicit"@, and @"order-only"@ respectively.
instance ToJSON DependencyType where
  toJSON NormalDependency    = "normal"
  toJSON ImplicitDependency  = "implicit"
  toJSON OrderOnlyDependency = "order-only"

-- | Inverse of the 'ToJSON' instance.
instance FromJSON DependencyType where
  parseJSON = (Aeson.withText "DependencyType" $ \case
                  "normal"     -> pure NormalDependency
                  "implicit"   -> pure ImplicitDependency
                  "order-only" -> pure OrderOnlyDependency
                  owise        -> [ "Invalid dependency type "
                                  , "\"", owise, "\"; should be one of "
                                  , "[\"normal\", \"implicit\", \"order-only\"]"
                                  ] |> mconcat |> T.unpack |> fail)

--------------------------------------------------------------------------------
