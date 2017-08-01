-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Tutorial.hs
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
--   Module      : Language.Ninja.Tutorial
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
module Language.Ninja.Tutorial
  ( -- * Introduction
    -- $introduction

    -- * Lexing
    -- $lexing

    -- * Parsing
    -- $parsing

    -- * Abstract Syntax Tree
    -- $ast

    -- * Compiling
    -- $compiling

    -- * Intermediate Representation
    -- $ir

    -- * Printing
    -- $printing

    -- * Executables
    -- $executables

    -- * Conclusion
    -- $conclusion
  ) where

import qualified Language.Ninja.AST        as AST
import qualified Language.Ninja.Compile    as Compile
import qualified Language.Ninja.Errors     as Errors
import qualified Language.Ninja.IR         as IR
import qualified Language.Ninja.Lexer      as Lexer
import qualified Language.Ninja.Misc       as Misc
import qualified Language.Ninja.Mock       as Mock
import qualified Language.Ninja.Parser     as Parser
import qualified Language.Ninja.Pretty     as Pretty

import           Data.Either               (either)

import           Control.Lens              (Iso', Lens', Prism')
import qualified Control.Lens              as Lens

import           Control.Monad.Error.Class (MonadError)

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM

import           Data.Versions             (Version)

--------------------------------------------------------------------------------

-- $introduction
--
-- This library contains most of the utilities you would ever want for working
-- with the <https://ninja-build.org Ninja> build language.
--
-- In general, when using @language-ninja@, you'll want the following imports:
--
-- @
-- import qualified "Language.Ninja.AST"        as AST
-- import qualified "Language.Ninja.IR"         as IR
-- import qualified "Language.Ninja.Errors"     as Errors
-- import qualified "Language.Ninja.Misc"       as Misc
-- import qualified "Language.Ninja.Mock"       as Mock
-- import qualified "Language.Ninja.Lexer"      as Lexer
-- import qualified "Language.Ninja.Parser"     as Parser
-- import qualified "Language.Ninja.Pretty"     as Pretty
-- import qualified "Language.Ninja.Compile"    as Compile
-- @
--
-- For this tutorial, we will also use some other imports:
--
-- @
-- import           "Control.Lens"              ('Iso'', 'Lens'', 'Prism'')
-- import qualified "Control.Lens"              as Lens
--
-- import           "Control.Monad.Error.Class" ('MonadError')
--
-- import           "Data.Either"               ('either')
--
-- import           "Data.Text"                 ('Text')
-- import qualified "Data.Text"                 as Text
--
-- import           "Data.HashSet"              ('HashSet')
-- import qualified "Data.HashSet"              as HS
--
-- import           "Data.HashMap.Strict"       ('HashMap')
-- import qualified "Data.HashMap.Strict"       as HM
--
-- import           "Data.Versions"             ('Version')
-- @

--------------------------------------------------------------------------------

-- $lexing
--
-- To lex a Ninja file, we use the "Language.Ninja.Lexer" module.
-- This results in a list of _annotated_ lexemes:
--
-- @lexFileIO "./build.ninja" :: IO (Lexer.'Lexer.Lexeme' Lexer.'Lexer.Ann')@
--
-- For more specialized use cases, consult the module documentation.

--------------------------------------------------------------------------------

-- $parsing
--
-- To parse a Ninja file, we use the "Language.Ninja.Parse" module.
-- In the simplest case, this amounts to parsing a file:
--
-- @ast <- (Parser.'Parser.parseFileIO' "./build.ninja") :: IO AST.'AST.Ninja'@
--
-- For more specialized use cases, consult the module documentation.

--------------------------------------------------------------------------------

-- $ast
--
-- Now that we have parsed the Ninja file, we can take a look at the AST:
--
-- @
-- let look lens = Lens.'Lens.view' lens ast
-- look AST.'AST.ninjaRules'     :: 'HashMap' 'Text' AST.'AST.Rule'
-- look AST.'AST.ninjaSingles'   :: 'HashMap' 'Text' AST.'AST.Build'
-- look AST.'AST.ninjaMultiples' :: 'HashMap' ('HashSet' 'Text') AST.'AST.Build'
-- look AST.'AST.ninjaPhonys'    :: 'HashMap' 'Text' ('HashSet' 'Text')
-- look AST.'AST.ninjaDefaults'  :: 'HashSet' 'Text'
-- look AST.'AST.ninjaPools'     :: 'HashMap' 'Text' 'Int'
-- look AST.'AST.ninjaSpecials'  :: 'HashMap' 'Text' 'Text'
-- @
--
-- [@AST.'AST.ninjaRules'@]
--   This field corresponds to the @rule@ declarations in the parsed Ninja file.
--   Specifically, it is a map from rule names (as 'Text') to @AST.'AST.Rule'@s.
--
-- [@AST.'AST.ninjaSingles'@]
--   This field contains the set of all non-phony @build@ declarations with
--   exactly one output. Specifically, it is a map from the build output name
--   to an @AST.'AST.Build'@.
--
-- [@AST.'AST.ninjaMultiples'@]
--   This field contains the set of all non-phony @build@ declarations with
--   two or more outputs. Specifically, it is a map from the set of outputs to
--   the corresponding @AST.'AST.Build'@.
--
-- [@AST.'AST.ninjaPhonys'@]
--   This field contains the set of all phony @build@ declarations, as a map
--   from the output name to the set of dependencies. If a phony @build@ has
--   multiple outputs, it will naturally be expanded to multiple entries in
--   this hash map.
--
-- [@AST.'AST.ninjaDefaults'@]
--   This field contains the set of all targets referenced in @default@
--   declarations.
--
-- [@AST.'AST.ninjaPools'@]
--   This field contains the set of all pools defined in the Ninja file,
--   represented as a mapping from the pool name to the pool depth.
--
-- [@AST.'AST.ninjaSpecials'@]
--   This field contains the set of all "special" top-level variables defined
--   in the Ninja file, as a mapping from the variable name to the variable
--   value. As it stands, this map will only ever have at most two keys:
--   @ninja_required_version@ and @builddir@. For more on these variables, look
--   at the manual <https://ninja-build.org/manual.html#ref_toplevel here>.
--
-- == @AST.'AST.Rule'@
--
-- A value of type @AST.'AST.Rule'@ is essentially the set of variables that
-- are bound in a @rule@ body, represented as a map from the variable ('Text')
-- to its unevaluated definition (@AST.'AST.Expr'@). The underlying 'HashMap'
-- can be extracted with @AST.'AST.ruleBind'@.
--
-- == @AST.'AST.Build'@
--
-- A value of type @AST.'AST.Build'@ contains four pieces of information:
--
-- [@AST.'AST.buildRule' :: 'Lens'' AST.'AST.Build' 'Text'@]
--   The name of the @rule@ associated with this @build@ declaration.
-- [@AST.'AST.buildDeps' :: 'Lens'' AST.'AST.Build' AST.'AST.Deps'@]
--   The set of dependencies for this @build@ declaration.
-- [@AST.'AST.buildEnv' :: 'Lens'' AST.'AST.Build' (AST.'AST.Env' 'Text' 'Text')@]
--   The set of file-level variables in scope when the @build@ declaration
--   was parsed.
-- [@AST.'AST.buildBind' :: 'Lens'' AST.'AST.Build' ('HashMap' 'Text' 'Text')@]
--   The set of bindings (indented @key = value@ pairs) for this @build@
--   declaration.
--
-- == @AST.'AST.Deps'@
--
-- A value of type @AST.'AST.Deps'@ contains three pieces of information:
--
-- [@AST.'AST.depsNormal' :: 'Lens'' AST.'AST.Deps' ('HashSet' 'Text')@]
--   The set of "normal" (explicit) dependencies for a @build@ declaration.
-- [@AST.'AST.depsImplicit' :: 'Lens'' AST.'AST.Deps' ('HashSet' 'Text')@]
--   The set of "implicit" dependencies for a @build@ declaration.
-- [@AST.'AST.depsOrderOnly' :: 'Lens'' AST.'AST.Deps' ('HashSet' 'Text')@]
--   The set of "order-only" dependencies for a @build@ declaration.
--
-- <https://ninja-build.org/manual.html#ref_dependencies This> section of the
-- Ninja manual describes in detail the differences between explicit, implicit,
-- and order-only dependencies.

--------------------------------------------------------------------------------

-- $compiling
--
-- To compile a Ninja AST (@AST.'AST.Ninja'@) to the Ninja intermediate
-- representation, we use @compile@ from the "Language.Ninja.Compile" module.
-- In the simplest case, this looks like:
--
-- @
-- let handleError :: Either Errors.'Errors.CompileError' a -> IO a
--     handleError = 'either' ('fail' . 'show') 'pure'
-- ir <- handleError (Compile.'Compile.compile' ast)
-- @
--
-- Since @Compile.'Compile.compile'@ returns in any monad with an instance
-- of @'MonadError' Errors.'Errors.CompileError'@, it is quite flexible.
--
-- For simplicity in this case, however, we use
-- @Either Errors.'Errors.CompileError' AST.'AST.Ninja'@
-- and convert to @IO@ by calling 'fail' when it fails.

--------------------------------------------------------------------------------

-- $ir
--
-- The @language-ninja@ intermediate representation, as defined in the
-- "Language.Ninja.IR" module, is a reduced form of the Ninja AST that handles
-- as much of the static semantics of Ninja as possible.
--
-- The Ninja IR does not have any notion of variables/scoping, does not contain
-- unrestricted hash maps or environments, and has the @rule@ declarations
-- inlined into the @build@ nodes, thus eliminating the "polymorphism"
-- associated with the @$in@ and @$out@ Ninja variables.
--
-- Since we have now compiled the Ninja AST, we can take a look at the IR:
--
-- @
-- let look lens = Lens.'Lens.view' lens ir
-- look IR.'IR.ninjaMeta'     :: IR.'IR.Meta'
-- look IR.'IR.ninjaBuilds'   :: 'HashSet' IR.'IR.Build'
-- look IR.'IR.ninjaPhonys'   :: 'HashMap' IR.'IR.Target' ('HashSet' IR.'IR.Target')
-- look IR.'IR.ninjaDefaults' :: 'HashSet' IR.'IR.Target'
-- look IR.'IR.ninjaPools'    :: 'HashSet' IR.'IR.Pool'
-- @
--
-- These fields correspond to more well-typed versions of their counterparts
-- in the AST. The main differences to note are:
--
-- 1. @AST.'AST.ninjaSpecials'@ is used to compute @IR.'IR.ninjaMeta'@.
-- 2. 'Text' has mostly been replaced with @IR.'IR.Target'@ where relevant.
-- 3. @AST.'AST.ninjaSingles'@ and @AST.'AST.ninjaMultiples'@ have been merged
--    into a single field: @IR.'IR.ninjaBuilds'@.
-- 4. @AST.'AST.ninjaRules'@ is gone; the @rule@s are now inlined into the
--    @build@s that use them.
--
-- == @IR.'IR.Meta'@
--
-- A value of type @IR.'IR.Meta'@ contains two pieces of information:
--
-- [@IR.'IR.metaReqVersion' :: 'Lens'' IR.'IR.Meta' (Maybe 'Version')@]
--   The parsed Ninja version required to build this file.
--   This corresponds to the @ninja_required_version@ top-level variable.
-- [@IR.'IR.metaBuildDir' :: 'Lens'' IR.'IR.Meta' (Maybe Misc.'Misc.Path')@]
--   This corresponds to the @builddir@ top-level variable.
--
-- == @IR.'IR.Build'@
--
-- A value of type @IR.'IR.Build'@ contains three pieces of information:
--
-- [@IR.'IR.buildRule' :: 'Lens'' IR.'IR.Build' IR.'IR.Rule'@]
--   The @rule@ associated with this @build@ declaration.
-- [@IR.'IR.buildOuts' :: 'Lens'' IR.'IR.Build' ('HashSet' IR.'IR.Output')@]
--   The set of outputs for this @build@ declaration.
-- [@IR.'IR.buildDeps' :: 'Lens'' IR.'IR.Build' ('HashSet' IR.'IR.Dependency')@]
--   The set of dependencies for this @build@ declaration.
--
-- == @IR.'IR.Rule'@
--
-- A value of type @IR.'IR.Rule'@ contains a lot of information. For brevity,
-- we will simply list the lens names and types; refer to the module
-- documentation ("Language.Ninja.IR.Rule") for more information.
--
-- @
-- IR.'IR.ruleName'         :: 'Lens'' IR.'IR.Rule' 'Text'
-- IR.'IR.ruleCommand'      :: 'Lens'' IR.'IR.Rule' Misc.'Misc.Command'
-- IR.'IR.ruleDescription'  :: 'Lens'' IR.'IR.Rule' (Maybe 'Text')
-- IR.'IR.rulePool'         :: 'Lens'' IR.'IR.Rule' IR.'IR.PoolName'
-- IR.'IR.ruleDepfile'      :: 'Lens'' IR.'IR.Rule' (Maybe Misc.'Misc.Path')
-- IR.'IR.ruleSpecialDeps'  :: 'Lens'' IR.'IR.Rule' (Maybe IR.'IR.SpecialDeps')
-- IR.'IR.ruleGenerator'    :: 'Lens'' IR.'IR.Rule' Bool
-- IR.'IR.ruleRestat'       :: 'Lens'' IR.'IR.Rule' Bool
-- IR.'IR.ruleResponseFile' :: 'Lens'' IR.'IR.Rule' (Maybe IR.'IR.ResponseFile')
-- @
--
-- == @IR.'IR.Pool'@
--
-- A value of type @IR.'IR.Pool'@ has a name (@IR.'IR.poolName'@) and a
-- depth (@IR.'IR.poolDepth'@). This type is correct-by-construction; it should
-- not be possible to construct a @'IR.Pool'@ that does not correspond to a
-- valid pool definition or reference.

--------------------------------------------------------------------------------

-- $printing
--
-- Currently there is a rudimentary pretty-printer for the lexemes and the AST
-- in the "Language.Ninja.Pretty" module. It simply returns 'Text' such that if
--
-- @
-- let pretty = 'pure' . Pretty.'Pretty.prettyNinja'
-- let parse  = Parser.'Parser.parseTextIO'
-- @
--
-- then @pretty >=> parse >=> pretty >=> parse@ should be the same as @pure@,
-- modulo read-only side effects and annotations.
--
-- There are plans to write a pretty-printer for the IR. This would be very
-- useful for generating Ninja.

--------------------------------------------------------------------------------

-- $executables
--
-- In addition to the library described above, this package also ships with
-- three executables: @ninja-lex@, @ninja-parse@, and @ninja-compile@.
-- These expose the corresponding module by using the Aeson instances to render
-- the lexed\/parsed\/compiled source.
--
-- == @ninja-lex@
--
-- The command-line interface for @ninja-lex@ looks like this:
--
-- > $ ninja-lex --help
-- > ninja-lex version 0.2.0
-- >
-- > Usage: ninja-lex (process | pretty)
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >
-- > Available commands:
-- >   process
-- >   pretty
--
-- > $ ninja-lex process --help
-- > Usage: ninja-lex process [--input FILEPATH] [--output FILEPATH]
-- >                          [--machine-readable]
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --input FILEPATH         Read the given FILEPATH as a Ninja file.
-- >   --output FILEPATH        Output to the given FILEPATH instead of /dev/stdout.
-- >   --machine-readable       Should the output be fully machine-readable?
--
-- > $ ninja-lex pretty --help
-- > Usage: ninja-lex pretty [--input FILEPATH] [--output FILEPATH] [--color]
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --input FILEPATH         Read the given FILEPATH as a Ninja file.
-- >   --output FILEPATH        Output to the given FILEPATH instead of /dev/stdout.
-- >   --color                  Should the output use ANSI color?
--
-- == @ninja-parse@
--
-- The command-line interface for @ninja-parse@ looks like this:
--
-- > $ ninja-parse --help
-- > ninja-parse version 0.2.0
-- >
-- > Usage: ninja-parse (process | pretty)
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >
-- > Available commands:
-- >   process
-- >   pretty
--
-- > $ ninja-parse process --help
-- > Usage: ninja-parse process [--input FILEPATH] [--output FILEPATH]
-- >                            [--machine-readable]
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --input FILEPATH         Read the given FILEPATH as a Ninja file.
-- >   --output FILEPATH        Output to the given FILEPATH instead of /dev/stdout.
-- >   --machine-readable       Should the output be fully machine-readable?
--
-- > $ ninja-parse pretty --help
-- > Usage: ninja-parse pretty [--input FILEPATH] [--output FILEPATH] [--color]
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --input FILEPATH         Read the given FILEPATH as a Ninja file.
-- >   --output FILEPATH        Output to the given FILEPATH instead of /dev/stdout.
-- >   --color                  Should the output use ANSI color?
--
-- == @ninja-compile@
--
-- The command-line interface for @ninja-compile@ looks like this:
--
-- > $ ninja-compile --help
-- > ninja-compile version 0.2.0
-- >
-- > Usage: ninja-compile [--input FILEPATH] [--output FILEPATH] [--machine-readable]
-- >
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --input FILEPATH         Read the given FILEPATH as a Ninja file.
-- >   --output FILEPATH        Output to the given FILEPATH instead of /dev/stdout.
-- >   --machine-readable       Should the output be fully machine-readable?

--------------------------------------------------------------------------------

-- $conclusion
--
-- I hope these tools will be useful to you for whatever task you want to do
-- with the Ninja language. Happy hacking!

--------------------------------------------------------------------------------
