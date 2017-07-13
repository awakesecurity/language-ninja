-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja.hs
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
--   Module      : Language.Ninja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Tools for parsing, pretty-printing, and compiling the Ninja build language.
--
--   This module re-exports some of the modules under the "Language.Ninja"
--   namespace for convenience.
--
--   Take a look at "Language.Ninja.Tutorial" for a tutorial on how to use this
--   library. If you just want to dive in, I recommend reading these modules
--   in the following order:
--
--   1. Skim "Language.Ninja.Misc", "Language.Ninja.Mock",
--      and "Language.Ninja.Errors" for familiarity with types and values
--      that are used all over this library.
--   2. Check out "Language.Ninja.Lexer" and then "Language.Ninja.Parser" to
--      get an idea for what is possible with the lexer/parser API.
--   3. Read "Language.Ninja.AST" to understand what the parsed AST looks like.
--   4. Read "Language.Ninja.Compile" to see what the AST-to-IR compiler does.
--   5. Read "Language.Ninja.IR" to understand what the compiled IR looks like.
--   6. Read "Language.Ninja.Pretty" to see what the pretty-printer is capable
--      of (spoiler: not very much).
--
--   If you are not already well-versed in the Ninja build language, it is also
--   probably worth reading the Ninja
--   <https://ninja-build.org/manual.html manual>.
--
--   When importing from this library, I recommend the following style:
--
--   > import qualified Language.Ninja     as Ninja
--   > import qualified Language.Ninja.AST as Ninja.AST
--   > import qualified Language.Ninja.IR  as Ninja.IR
--
--   Happy hacking!
--
--   @since 0.1.0
module Language.Ninja
  ( module Language.Ninja.Compile
  , module Language.Ninja.Lexer
  , module Language.Ninja.Parser
  , module Language.Ninja.Pretty
  , module Language.Ninja.Misc
  , module Language.Ninja.Mock
  ) where

import           Language.Ninja.Compile
import           Language.Ninja.Lexer
import           Language.Ninja.Misc
import           Language.Ninja.Mock
import           Language.Ninja.Parser
import           Language.Ninja.Pretty
