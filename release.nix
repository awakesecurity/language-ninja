{ nixpkgs ? <nixpkgs>, nixpkgsArgs ? {} }:

{
  language-ninja-ghc7103 = import ./default.nix { compiler = "ghc7103"; };
  language-ninja-ghc802  = import ./default.nix { compiler = "ghc802";  };
  language-ninja-ghc821  = import ./default.nix { compiler = "ghc821";  };
}
