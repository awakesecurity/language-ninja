{ mkDerivation, aeson, aeson-pretty, base, bytestring, Cabal
, cabal-doctest, concurrent-supply, containers, deepseq, doctest
, flow, ghc, haddock-api, haddock-library, hashable, here, intern
, lens, makefile, megaparsec, monad-mock, mtl, optparse-generic
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, smallcheck, stdenv, system-filepath, tasty
, tasty-html, tasty-hunit, tasty-lens, tasty-quickcheck
, tasty-smallcheck, template-haskell, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.0.1";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq flow hashable intern lens
    megaparsec mtl QuickCheck quickcheck-instances smallcheck
    system-filepath text transformers unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring concurrent-supply containers
    flow hashable here lens makefile mtl optparse-generic prettyprinter
    prettyprinter-ansi-terminal text transformers unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring cabal-doctest containers doctest flow ghc
    haddock-api haddock-library hashable lens monad-mock mtl QuickCheck
    quickcheck-instances smallcheck system-filepath tasty tasty-html
    tasty-hunit tasty-lens tasty-quickcheck tasty-smallcheck
    template-haskell text transformers turtle unordered-containers
    versions
  ];
  homepage = "https://github.com/awakesecurity/language-ninja";
  description = "A library for dealing with the Ninja build language";
  license = stdenv.lib.licenses.asl20;
}
