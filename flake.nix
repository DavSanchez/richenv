{
  description = "richenv - Rich environment variable setup for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
    devenv,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
        haskell = pkgs.haskellPackages; # Replaceable with pkgs.haskell.packages.ghc96 or similar
      in {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              actionlint.enable = true;
              alejandra.enable = true;
              cabal-fmt.enable = true;
              cabal2nix.enable = true;
              convco.enable = true;
              ormolu.enable = true;
              hlint.enable = true;
              hpack.enable = true;
              # yamllint.enable = true;
              # hunspell.enable = true;
            };
            settings = {
              alejandra.exclude = ["default.nix"];
            };
          };
        };
        devShells = {
          default = pkgs.mkShell {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
            buildInputs = with haskell; [
              ghc
              cabal-install
              haskell-language-server
              hspec-discover
            ];
          };
          # devenv = devenv.lib.mkShell {};
        };
        packages = {
          default = haskell.callPackage ./default.nix {};
        };
      }
    );
}
