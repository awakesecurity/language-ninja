{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = import nixpkgs nixpkgsArgs;

  inherit (pkgs) haskell;

  addHydraHaddock = hp: pname: (
    with rec {
      pkg = hp.${pname};
      suffix = "share/doc/${pkg.system}-${hp.ghc.name}/${pkg.name}/html";
    };

    haskell.lib.overrideCabal pkg (old: rec {
      postInstall = ((old.postInstall or "") + ''
        mkdir -p "$out/nix-support"
        echo "doc haddock $out/${suffix} index.html" \
            > "$out/nix-support/hydra-build-products"
      '');
    }));


  hp = haskell.packages.${compiler}.override {
    overrides = self: super: {
      makefile       = self.callPackage ./nix/makefile.nix {};
      versions       = self.callPackage ./nix/versions.nix {};
      language-ninja = self.callPackage ./nix/language-ninja.nix {};
    };
  };
};

{
  language-ninja = addHydraHaddock hp "language-ninja";
}
