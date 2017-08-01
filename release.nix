{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {} }:

{
  ghc7103 = import ./default.nix { compiler = "ghc7103"; };
  ghc802  = import ./default.nix { compiler = "ghc802";  };
  ghc821  = import ./default.nix { compiler = "ghc821";  };
}
