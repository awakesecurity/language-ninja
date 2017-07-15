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

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

-- |
--   Module      : Language.Ninja.Misc
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Utility types and functions for the rest of @language-ninja@.
--
--   This module re-exports all of the modules under the "Language.Ninja.Misc"
--   namespace for convenience.
--
--   It is recommended that you import it with the following style:
--
--   > import qualified Language.Ninja.Misc as Misc
--
--   @since 0.1.0
module Language.Ninja.Misc
  ( -- * "Language.Ninja.Misc.Command"
    Misc.Command, Misc.makeCommand
  , Misc.commandText

    -- * "Language.Ninja.Misc.Path"
  , Misc.Path, Misc.makePath
  , Misc.pathIText, Misc.pathText, Misc.pathString, Misc.pathFP

    -- * "Language.Ninja.Misc.IText"
  , Misc.IText, Misc.uninternText, Misc.internText, Misc.itext

    -- * "Language.Ninja.Misc.Positive"
  , Misc.Positive, Misc.makePositive, Misc.fromPositive

    -- * "Language.Ninja.Misc.Annotated"
  , Misc.Annotated (..), Misc.annotation

    -- * "Language.Ninja.Misc.Located"
  , Misc.Located, Misc.tokenize, Misc.tokenizeFile, Misc.tokenizeText
  , Misc.locatedPos, Misc.locatedVal

  , Misc.Spans, Misc.makeSpans
  , Misc.spansSet

  , Misc.Span, Misc.makeSpan
  , Misc.spanPath, Misc.spanRange, Misc.spanStart, Misc.spanEnd

  , Misc.Position, Misc.makePosition
  , Misc.positionFile, Misc.positionOffset, Misc.positionLine, Misc.positionCol
  , Misc.comparePosition

  , Misc.Offset
  , Misc.compareOffset, Misc.offsetLine, Misc.offsetColumn
  , Misc.Line, Misc.Column
  ) where

import qualified Language.Ninja.Misc.Annotated as Misc
import qualified Language.Ninja.Misc.Command   as Misc
import qualified Language.Ninja.Misc.IText     as Misc
import qualified Language.Ninja.Misc.Located   as Misc
import qualified Language.Ninja.Misc.Path      as Misc
import qualified Language.Ninja.Misc.Positive  as Misc
