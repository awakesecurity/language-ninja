{ mkDerivation, aeson, aeson-diff, aeson-pretty, base, bytestring
, containers, cryptonite, deepseq, directory, Earley, exceptions
, extra, flow, hashable, HUnit, intern, lens, makefile, megaparsec
, mtl, QuickCheck, quickcheck-instances, shake, smallcheck
, smallcheck-lens, stdenv, system-filepath, tasty, tasty-golden
, tasty-html, tasty-hunit, tasty-lens, tasty-quickcheck
, tasty-smallcheck, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.0.1";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers deepseq directory
    Earley exceptions extra flow hashable intern lens makefile
    megaparsec mtl QuickCheck quickcheck-instances shake smallcheck
    system-filepath text transformers unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring containers cryptonite flow
    hashable lens makefile text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-diff aeson-pretty base bytestring containers flow
    hashable HUnit lens mtl QuickCheck quickcheck-instances smallcheck
    smallcheck-lens system-filepath tasty tasty-golden tasty-html
    tasty-hunit tasty-lens tasty-quickcheck tasty-smallcheck text
    transformers turtle unordered-containers versions
  ];
  homepage = "https://github.com/awakesecurity/language-ninja";
  description = "A Haskell library for parsing, pretty-printing, and compiling the Ninja build language";
  license = stdenv.lib.licenses.asl20;
}
