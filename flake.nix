{
  description = "richenv - Rich environment variable setup for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks.url = "github:cachix/git-hooks.nix";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.git-hooks.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {
        config,
        pkgs,
        ...
      }: let
        haskell = pkgs.haskellPackages;
      in {
        pre-commit = {
          check.enable = true;
          settings = {
            hooks = {
              actionlint.enable = true;
              alejandra = {
                enable = true;
                excludes = ["default.nix"];
              };
              cabal-fmt.enable = true;
              cabal2nix.enable = true;
              convco.enable = true;
              ormolu.enable = true;
              hlint.enable = true;
              hpack.enable = true;
              # yamllint.enable = true;
              # hunspell.enable = true;
            };
          };
        };

        devShells.default = pkgs.mkShell {
          shellHook = ''
            ${config.pre-commit.installationScript}
            echo 1>&2 "Welcome to the development shell!"
          '';
          nativeBuildInputs = config.pre-commit.settings.enabledPackages;

          buildInputs = with haskell; [
            ghc
            cabal-install
            haskell-language-server
            hspec-discover
          ];
        };

        packages.default = haskell.callPackage ./default.nix {};
      };
    };
}
