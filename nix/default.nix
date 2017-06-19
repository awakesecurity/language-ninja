{ mkDerivation, aeson, aeson-diff, aeson-pretty, base, bytestring
, containers, cryptonite, deepseq, directory, exceptions, extra
, flow, hashable, hspec, HUnit, intern, lens, makefile, megaparsec
, mtl, QuickCheck, shake, stdenv, system-filepath, text
, transformers, turtle, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers deepseq directory
    exceptions extra flow hashable intern lens makefile megaparsec mtl
    QuickCheck shake system-filepath text transformers
    unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring containers cryptonite flow
    hashable lens makefile QuickCheck text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-diff aeson-pretty base bytestring hspec HUnit
    system-filepath text transformers turtle
  ];
  homepage = "https://github.com/awakenetworks/language-ninja";
  description = "A Haskell library for parsing, pretty-printing, and evaluating the Ninja build language";
  license = stdenv.lib.licenses.asl20;
}
