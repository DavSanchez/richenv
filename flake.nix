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
      imports = with inputs; [
        git-hooks.flakeModule
        # haskell-flake.flakeModule
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
      }: {
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
            };
          };
        };

        devShells.default = pkgs.mkShell {
          shellHook = ''
            ${config.pre-commit.installationScript}
            echo 1>&2 "Welcome to the development shell!"
          '';
          nativeBuildInputs = config.pre-commit.settings.enabledPackages;

          buildInputs = with pkgs.haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hspec-discover
          ];
        };

        packages = {
          default = pkgs.haskellPackages.callPackage ./default.nix {};

          richenv-ghc94 = pkgs.haskell.packages.ghc94.callPackage ./default.nix {};
          richenv-ghc96 = pkgs.haskell.packages.ghc96.callPackage ./default.nix {};
          richenv-ghc98 = pkgs.haskell.packages.ghc98.callPackage ./default.nix {};
          # richenv-ghc910 = pkgs.haskell.packages.ghc910.callPackage ./default.nix {};
        };
      };
    };
}
