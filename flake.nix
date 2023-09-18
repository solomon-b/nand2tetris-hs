{
  description = "Nand2Tetris HDL Language";

  inputs = {
    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-23.05;
    };

    flake-utils = {
      url = github:numtide/flake-utils;
    };
  };

  outputs = { self, nixpkgs, unstable, flake-utils }:
    let
      overlay = import ./overlay.nix;
      overlays = [ overlay ];
    in flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.n2t-hdl ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.haskell-language-server
            pkgs.nixfmt
          ];
        };
        packages.default = pkgs.haskellPackages.n2t-hdl;
      }) // {
        overlays.default = overlay;
      };
}
