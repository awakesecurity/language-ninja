{ mkDerivation, attoparsec, base, doctest, Glob, QuickCheck, stdenv
, text
}:
mkDerivation {
  pname = "makefile";
  version = "1.0.0.3";
  sha256 = "0w36rxzx4ryhrmjayqm9fad2zpkxnq4dmpxshd8q2x8wa3wp8j5p";
  libraryHaskellDepends = [ attoparsec base text ];
  testHaskellDepends = [
    attoparsec base doctest Glob QuickCheck text
  ];
  homepage = "http://github.com/nmattia/mask";
  description = "Simple Makefile parser and generator";
  license = stdenv.lib.licenses.mit;
}
