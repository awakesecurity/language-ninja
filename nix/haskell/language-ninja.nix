{ mkDerivation, aeson, aeson-pretty, base, bytestring
, concurrent-supply, containers, deepseq, docopt, flow, ghc
, haddock-api, haddock-library, hashable, intern, lens, makefile
, megaparsec, monad-mock, mtl, prettyprinter
, prettyprinter-ansi-terminal, QuickCheck, quickcheck-instances
, smallcheck, stdenv, system-filepath, tasty, tasty-html
, tasty-hunit, tasty-lens, tasty-quickcheck, tasty-smallcheck, text
, transformers, turtle, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.0.1";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq flow hashable intern lens
    megaparsec mtl QuickCheck quickcheck-instances smallcheck
    system-filepath text transformers unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring concurrent-supply containers
    docopt flow hashable lens makefile mtl prettyprinter
    prettyprinter-ansi-terminal text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers flow ghc haddock-api
    haddock-library hashable lens monad-mock mtl QuickCheck
    quickcheck-instances smallcheck system-filepath tasty tasty-html
    tasty-hunit tasty-lens tasty-quickcheck tasty-smallcheck text
    transformers turtle unordered-containers versions
  ];
  homepage = "https://github.com/awakesecurity/language-ninja";
  description = "A library for dealing with the Ninja build language";
  license = stdenv.lib.licenses.asl20;
}
