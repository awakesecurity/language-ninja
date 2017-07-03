{ mkDerivation, base, constraints, exceptions, haskell-src-exts
, haskell-src-meta, hspec, monad-control, mtl, stdenv
, template-haskell, th-orphans, transformers-base
}:
mkDerivation {
  pname = "monad-mock";
  version = "0.1.1.1";
  sha256 = "1ja9s1z4y1vrk01bgav83cj95hzp8mrwil74b7lmc4fmdmr7a5y3";
  libraryHaskellDepends = [
    base constraints exceptions haskell-src-exts haskell-src-meta
    monad-control mtl template-haskell th-orphans transformers-base
  ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/cjdev/monad-mock#readme";
  description = "A monad transformer for mocking mtl-style typeclasses";
  license = stdenv.lib.licenses.isc;
}
