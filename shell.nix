{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with {
  pkgs = (import nixpkgs) nixpkgsArgs;
  drv = import ./release.nix { inherit nixpkgs nixpkgsArgs compiler; };
};

if pkgs.lib.inNixShell then drv.env else drv
