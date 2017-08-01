{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = (import nixpkgs) nixpkgsArgs;

  inherit (pkgs) haskell;

  # Compute, e.g.: "x86_64-linux-ghc-8.0.2"
  computeHaskellDir = hp: pkg: "${pkg.system}-${hp.ghc.name}";

  setHaddockStyle = hp: pkg: (
    with rec {
      fetchJQuery = { version, sha256 }: pkgs.fetchurl {
        url = "https://code.jquery.com/jquery-${version}.min.js";
        inherit sha256;
      };

      jquery = fetchJQuery {
        version = "3.2.1";
        sha256  = "1pl2imaca804jkq29flhbkfga5qqk39rj6j1n179h5b0rj13h247";
      };
    };

    haskell.lib.overrideCabal pkg (old: rec {
      preInstall = (''
        for haddockDir in ./dist/doc/html/*; do
             if test -d "$haddockDir"; then
                 rm -fv "$haddockDir/ocean.css"
                 rm -fv "$haddockDir/haddock-util.js"
                 cp -v "./misc/haddock.css" "$haddockDir/ocean.css"
                 cp -v "./misc/haddock.js"  "$haddockDir/haddock-util.js"
                 ln -sv ${jquery} "$haddockDir/jquery.js"
             fi
        done
      '' + (old.preInstall or ""));
    }));

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
      with { cp = file: (self.callPackage (./nix/haskell + "/${file}") {}); };

      {
        makefile                    = cp "makefile.nix";
        monad-mock                  = cp "monad-mock.nix";
        prettyprinter               = cp "prettyprinter.nix";
        prettyprinter-ansi-terminal = cp "prettyprinter-ansi-terminal.nix";
        smallcheck-lens             = doJailbreak super.smallcheck-lens;
        tasty-lens                  = doJailbreak super.tasty-lens;
        versions                    = cp "versions.nix";

        language-ninja = (cp "language-ninja.nix").overrideDerivation (old:
          with {
            sf = name: type: let bn = baseNameOf (toString name); in !(
              (type == "directory" && (bn == ".git"))
              || pkgs.lib.hasSuffix "~" bn
              || pkgs.lib.hasSuffix ".o" bn
              || pkgs.lib.hasSuffix ".so" bn
              || pkgs.lib.hasSuffix ".nix" bn
              || (type == "symlink" && pkgs.lib.hasPrefix "result" bn)
            );
          };
          { src = builtins.filterSource sf ./.; });
      }
    );
  };
};

setHaddockStyle hp
  (addHydraHaddock hp
  (addHydraHPC hp
  (addHydraTasty hp hp.language-ninja)))
