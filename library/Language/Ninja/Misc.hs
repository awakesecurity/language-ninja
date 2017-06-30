-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Misc.hs
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

-- |
--   Module      : Language.Ninja.Misc
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Misc
  ( -- * @Command@
    Misc.Command, Misc.makeCommand
  , Misc.commandText

    -- * @Path@
  , Misc.Path, Misc.makePath
  , Misc.pathIText, Misc.pathText, Misc.pathString, Misc.pathFP

    -- * @IText@
  , Misc.IText, Misc.uninternText, Misc.internText, Misc.itext

    -- * @Positive@
  , Misc.Positive, Misc.makePositive, Misc.fromPositive

    -- * @Located@
  , Misc.Located, Misc.tokenize, Misc.tokenizeFile, Misc.tokenizeText
  , Misc.locatedPos, Misc.locatedVal

    -- * @Position@
  , Misc.Position, Misc.makePosition
  , Misc.positionFile, Misc.positionLine, Misc.positionCol
  , Misc.Line, Misc.Column
  ) where

import qualified Language.Ninja.Misc.Command  as Misc
import qualified Language.Ninja.Misc.IText    as Misc
import qualified Language.Ninja.Misc.Located  as Misc
import qualified Language.Ninja.Misc.Path     as Misc
import qualified Language.Ninja.Misc.Positive as Misc
