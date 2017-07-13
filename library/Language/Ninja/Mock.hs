-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Mock.hs
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

-- |
--   Module      : Language.Ninja.Mock
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Typeclasses that allow mocking via dependency injection (i.e.: free monads
--   or similar techniques) in @language-ninja@.
--
--   This module re-exports all of the modules under the "Language.Ninja.Mock"
--   namespace for convenience.
--
--   It is recommended that you import it with the following style:
--
--   > import qualified Language.Ninja.Mock as Mock
--
--   @since 0.1.0
module Language.Ninja.Mock
  ( -- * "Language.Ninja.Mock.ReadFile"
    Mock.MonadReadFile (..)
  ) where

import qualified Language.Ninja.Mock.ReadFile as Mock
