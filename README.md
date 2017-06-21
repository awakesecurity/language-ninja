# `language-ninja`

[![Hackage][hackage-badge]][hackage-link]
[![Stackage][stackage-badge]][stackage-link]
[![License][license-badge]][license-link]

`language-ninja` is a Haskell library for parsing, pretty-printing, and
evaluating the [Ninja build language](https://ninja-build.org).

## Motivation

`language-ninja` was written as a part of Awake Security's efforts in creating
incremental build infrastructure for the Nix package manager, and in particular
for Haskell packages. This library is the basis for a tool, `ninja2nix`, that,
given a Ninja file, will output a JSON file containing information that a Nix
function can use to compute a derivation representing an incremental build
using one derivation per build edge in the parsed Ninja build graph.

In conjunction with an as-of-yet unwritten tool, `cabal2ninja`, that will
generate a Ninja file based on information from `cabal` and `ghc -M`, we will
have incremental builds for any Haskell package that uses a default `Setup.hs`
file.

Using Ninja as an intermediate representation has advantages here, since it
means that `ninja2nix` could be useful for building other projects that use
a build system that can output Ninja, like the Linux kernel (with `kninja`),
Chromium, any CMake project, any Bazel project, or some Make-based projects
(with `kati`).

## Parsing

The lexer/parser that `language-ninja` currently uses is from Neil Mitchell's
`shake` project. It is optimized for performance, but is not obviously correct
and does not have very good diagnostics, so there are plans to move to a
`megaparsec`-based parser. This would also allow for the addition of location
information to the parsed AST. In my view, it is very important that a parser
output an AST that can be pretty-printed exactly to same sequence of bytes that
were in the parsed file, as this makes testing and diagnostics much easier to
write.

## Pretty-printing

Currently there is a rudimentary pretty-printer in `Language.Ninja.Pretty`.
It simply outputs text such that if that text is parsed and the parsed data
is pretty-printed again, the resulting text will be identical to the original
text (this is tested on a variety of Ninja files in the test suite).

## Evaluating

The `Ninja` type from `Language.Ninja.AST.Ninja` contains precisely the data
that must be acted on dynamically in a Ninja. In converting from a `PNinja`
from the parser to a `Ninja` (using `Language.Ninja.Eval.evaluate`), you are
eliminating all statically-dischargeable language features in Ninja, like
variables. It also "monomorphizes" Ninja `rule`s, since rule-level `$out`
references are a kind of parametric polymorphism. This `Ninja` type is thus
far more suitable for processing than the `PNinja` type.

<!----------------------------------------------------------------------------->

[hackage-badge]:
    https://img.shields.io/hackage/v/language-ninja.svg?label=Hackage
[hackage-link]:
    https://hackage.haskell.org/package/language-ninja
[stackage-badge]:
    https://www.stackage.org/package/language-ninja/badge/lts?label=Stackage
[stackage-link]:
    https://www.stackage.org/package/language-ninja
[license-badge]:
    https://img.shields.io/badge/License-Apache%202.0-blue.svg
[license-link]:
    https://spdx.org/licenses/Apache-2.0.html
