{ mkDerivation, ansi-wl-pprint, base, bytestring, criterion
, doctest, mtl, pgp-wordlist, QuickCheck, random, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.1";
  sha256 = "0bksn65rvnc0f59mfzhyl9yaccfh5ap6jxj1r477izlnkfs0k03y";
  revision = "1";
  editedCabalFile = "bead6f9fcf9551e4470abee2830c3597e3e6439d016c97a28d371158ca1a6e2c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base criterion mtl random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible prettyprinter";
  license = stdenv.lib.licenses.bsd2;
}
