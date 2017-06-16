{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = import nixpkgs nixpkgsArgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      makefile = self.callPackage ./nix/makefile.nix {};
      versions = self.callPackage ./nix/versions.nix {};
    };
  };
};

{
  language-ninja = haskellPackages.callPackage ./nix/language-ninja.nix {};
}
