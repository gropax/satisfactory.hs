{
  description = "Satisfactory optimisation solver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPkgs = pkgs.haskell.packages.ghc967;

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.bashInteractive
            pkgs.glibcLocales

            haskellPkgs.ghc
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            haskellPkgs.hlint
            haskellPkgs.ormolu
          ];

          shellHook = ''
            export SHELL=${pkgs.bashInteractive}/bin/bash
          '';
        };
      });
}

