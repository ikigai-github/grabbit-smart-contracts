{
  description = "vulcan-smart-contracts";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]Vulcan \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    # general inputs
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-upstream.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "plutarch/nixpkgs";
    };

    ply = {
      url = "github:mlabs-haskell/ply?rev=f77e877efa65b9aa5f1b9f64cdfcc28e51b46f9e";
      inputs.extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    };

    # onchain inputs
    plutarch.url = "github:Plutonomicon/plutarch?rev=979471de3c02cca61b58b71e96dc880cd8061ab7";
    haskell-nix-onchain.follows = "plutarch/haskell-nix";

    plutarch-unit.url = "github:Liqwid-Labs/plutarch-unit?rev=bc5c07d47fecef2725081445e373655855c9bbd4";
  };


  outputs = inputs@{ self, nixpkgs, nixpkgs-upstream, ply, plutarch, plutarch-unit, pre-commit-hooks, ... }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      onchainNixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ inputs.haskell-nix-onchain.overlay (import "${plutarch.inputs.iohk-nix}/overlays/crypto") ];
      };

      pureNixpkgsFor = system: import nixpkgs-upstream { inherit system; };

      ctlNixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ inputs.cardano-transaction-lib.overlay.${system} ];
      };

      fourmoluFor = system: (pureNixpkgsFor system).haskell.packages.ghc923.fourmolu;

      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;

            settings = {
              ormolu.defaultExtensions = [
                "ImportQualifiedPost"
                "TypeApplications"
                "OverloadedRecordDot"
                "BangPatterns"
              ];
            };

            hooks = {
              fourmolu.enable = true;
              hlint.enable = true;
              nixpkgs-fmt.enable = true;
            };

            tools = {
              fourmolu = fourmoluFor system;
              hlint = (pureNixpkgsFor system).haskellPackages.hlint_3_4_1;
            };
          };

      # ONCHAIN / Plutarch

      onchain = rec {
        ghcVersion = "ghc923";
        myhackages = system: compiler-nix-name: inputs.haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name (
          [
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${plutarch}/plutarch-test"
            "${plutarch-unit.inputs.liqwid-plutarch-extra}"
            "${plutarch-unit.inputs.plutarch-numeric}"
            "${plutarch-unit.inputs.plutarch-quickcheck}"
            "${plutarch-unit.inputs.plutarch-context-builder}"
            "${plutarch-unit}"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
          ]
        );
        projectFor = system:
          let
            pkgs = onchainNixpkgsFor system;
            pkgs' = pureNixpkgsFor system;
            hackages = myhackages pkgs.system ghcVersion;
          in
          pkgs.haskell-nix.cabalProject' (inputs.plutarch.applyPlutarchDep pkgs {
            src = ./.;
            compiler-nix-name = ghcVersion;
            inherit (hackages) extra-hackages extra-hackage-tarballs modules;
            cabalProjectFileName = "cabal.project.onchain";
            shell = {
              withHoogle = true;
              exactDeps = true;
              nativeBuildInputs = [
                pkgs'.cabal-install
                pkgs'.hlint
                pkgs'.haskellPackages.cabal-fmt
                pkgs'.nixpkgs-fmt
                inputs.plutarch.project.${system}.hsPkgs.hspec-discover.components.exes.hspec-discover
                (fourmoluFor system)
                (inputs.plutarch.hlsFor ghcVersion system)
              ];
              shellHook = ''
                export NIX_SHELL_TARGET="onchain"
                ln -fs cabal.project.onchain cabal.project
              '' + (preCommitCheckFor system).shellHook;
            };
          });
      };

    in
    {
      inherit onchainNixpkgsFor;

      onchain = {
        project = perSystem onchain.projectFor;
        flake = perSystem (system: (onchain.projectFor system).flake { });
      };

      packages = perSystem (system:
        self.onchain.flake.${system}.packages
      );

      checks = perSystem (system:
        self.onchain.flake.${system}.checks
        // { formatting-checks = preCommitCheckFor system; }

      );

      herculesCI.ciSystems = [ "x86_64-linux" ];

      check = perSystem (system:
        (onchainNixpkgsFor system).runCommand "combined-test"
          {
            checksss =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system}
              ++ [
                self.devShells.${system}.onchain.inputDerivation
              ];
          } ''
          echo $checksss
          touch $out
        ''
      );

      devShells = perSystem (system: {
        onchain = self.onchain.flake.${system}.devShell;
      });

      nixosModules.vulcan-server = { pkgs, ... }: {
        imports = [ ./nix/vulcan-server-service.nix ];
        nixpkgs.overlays = [
          (_: _: {
            vulcan-server = self.packages.${pkgs.system}."vulcan-scripts:exe:vulcan-server";
          })
        ];
      };
    };
}
