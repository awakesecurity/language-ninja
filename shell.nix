{ nixpkgs ? import ./nix/nixpkgs.nix, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = (import nixpkgs) nixpkgsArgs;
  drv = import ./default.nix { inherit nixpkgs nixpkgsArgs compiler; };
};

if pkgs.lib.inNixShell then drv.env else drv
