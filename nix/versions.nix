{ mkDerivation, base, deepseq, hashable, megaparsec, microlens
, stdenv, tasty, tasty-hunit, text
}:
mkDerivation {
  pname = "versions";
  version = "3.1.1";
  sha256 = "1pnmbvlchjskavp6h04xdxwxg61aplqpxnawnbzflyf1mvpz0dm4";
  libraryHaskellDepends = [ base deepseq hashable megaparsec text ];
  testHaskellDepends = [ base microlens tasty tasty-hunit text ];
  description = "Types and parsers for software version numbers";
  license = stdenv.lib.licenses.bsd3;
}
