{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = import nixpkgs nixpkgsArgs;

  inherit (pkgs) haskell;

  # Compute, e.g.: "x86_64-linux-ghc-8.0.2"
  computeHaskellDir = hp: pkg: "${pkg.system}-${hp.ghc.name}";

  addHydraHaddock = hp: pkg: (
    with rec {
      suffix = "share/doc/${computeHaskellDir hp pkg}/${pkg.name}/html";
    };

    haskell.lib.overrideCabal pkg (old: rec {
      doHaddock = true;
      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support"
        echo "doc haddock $out/${suffix} index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  addHydraTasty = hp: pkg: (
    with rec {
      dir = computeHaskellDir hp hp.tasty-html;
      data = "${hp.tasty-html}/share/${dir}/${hp.tasty-html.name}/data/";
      static = pkgs.runCommand "tasty-html-static" {} ''
        mkdir -pv "$out"
        ln -sv ${data}/jquery-*.min.js                      "$out/"
        ln -sv ${data}/bootstrap/dist/css/bootstrap.min.css "$out/"
        ln -sv ${data}/bootstrap/dist/js/bootstrap.min.js   "$out/"
      '';
    };

    haskell.lib.overrideCabal pkg (old: rec {
      testTarget = "--test-options=\"--html tasty.html --assets static\"";
      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support/test-results"
        mv -v tasty.html "$out/nix-support/test-results/index.html"
        ln -sv ${static} "$out/nix-support/test-results/static"
        echo "report tests $out/nix-support/test-results index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  addHydraHPC = hp: pkg: (
    haskell.lib.overrideCabal pkg (old: rec {
      doCoverage = true;

      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support"
        echo "report hpc $out/share/hpc/dyn/html/${pkg.name} hpc_index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  hp = haskell.packages.${compiler}.override {
    overrides = self: super: (
      with haskell.lib;
      with { cp = file: self.callPackage file {}; };

      {
        makefile                    = cp ./nix/makefile.nix;
        versions                    = cp ./nix/versions.nix;
        prettyprinter               = cp ./nix/prettyprinter.nix;
        prettyprinter-ansi-terminal = cp ./nix/prettyprinter-ansi-terminal.nix;
        smallcheck-lens             = doJailbreak super.smallcheck-lens;
        tasty-lens                  = doJailbreak super.tasty-lens;
        language-ninja              = cp ./nix/language-ninja.nix;
      }
    );
  };
};

{
  language-ninja = (
    addHydraHaddock hp
    (addHydraHPC hp
    (addHydraTasty hp hp.language-ninja)));
}
