{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = import nixpkgs nixpkgsArgs;

  inherit (pkgs) haskell;

  # Compute, e.g.: "x86_64-linux-ghc-8.0.2"
  computeHaskellDir = ghc: pkg: "${pkg.system}-${ghc.name}";
  
  addHydraHaddock = ghc: pkg: (
    with rec {
      suffix = "share/doc/${computeHaskellDir ghc pkg}/${pkg.name}/html";
    };

    haskell.lib.overrideCabal pkg (old: rec {
      postInstall = ((old.postInstall or "") + ''
        mkdir -p "$out/nix-support"
        echo "doc haddock $out/${suffix} index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  addHydraTasty = ghc: pkg: (
    with rec {
      dir = computeHaskellDir ghc pkg;
      data = "${hp.tasty-html}/share/${dir}/${hp.tasty-html.name}/data/";
      static = pkgs.runCommand "tasty-html-static" {} ''
        mkdir -pv "$out"
        ln -sv  ${data}/jquery-*.min.js                      "$out/"
        ln -sv  ${data}/bootstrap/dist/css/bootstrap.min.css "$out/"
        ln -sv  ${data}/bootstrap/dist/js/bootstrap.min.js   "$out/"
      '';
    };

    haskell.lib.overrideCabal pkg (old: rec {
      testTarget = "--test-options=\"--html tasty.html --assets static\"";
      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support/test-results"
        mv -v tasty.html "$out/nix-support/test-results/"
        ln -sv ${static} "$out/nix-support/test-results/static"
        echo "report Tests $out/nix-support/test-results tasty.html" \
            >> "$out/nix-support/hydra-build-products"
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
  language-ninja = (
    addHydraTasty hp.ghc (addHydraHaddock hp.ghc hp.language-ninja));
}
