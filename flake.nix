{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Update this more frequently than unstanble. Updating unstable frequently breaks stuff during work
    nixpkgs-edge.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  inputs.yafl = {
    url = "github:gjhenrique/yafl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    dream2nix,
    home-manager,
    nixpkgs,
    nixpkgs-unstable,
    nixpkgs-edge,
    self,
    yafl,
    ...
  } @ inputs: let
    linux = "x86_64-linux";
    mac = "aarch64-darwin";
    systems = [linux mac];

    forAllSystems = nixpkgs.lib.genAttrs systems;

    pkgsFor = forAllSystems (system:
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      });

    pkgsUnstableFor = forAllSystems (system:
      import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      });

    pkgsEdgeFor = forAllSystems (system:
      import nixpkgs-edge {
        inherit system;
        config.allowUnfree = true;
      });

    secrets = builtins.fromJSON (builtins.readFile ./secrets/secrets.json);
  in {
    formatter = forAllSystems (system: pkgsFor.${system}.alejandra);

    devShells = forAllSystems (
      system: let
        pkgs = pkgsFor.${system};
        scripts = {
          fmt = pkgs.writeScriptBin "fmt-all" ''
            #!${pkgs.bash}/bin/bash

            git ls-files '*.nix' | xargs nix fmt
          '';

          hm = pkgs.writeScriptBin "hm" ''
            #!${pkgs.bash}/bin/bash

            os=linux

            if [[ "$(uname)" == "Darwin" ]]; then
                os=darwin
            fi

            nix run  -- home-manager switch -b backup --flake ".#guilherme@$os"
          '';

          nixos = pkgs.writeScriptBin "nixos" ''
            #!${pkgs.bash}/bin/bash

            sudo nixos-rebuild switch --flake .
          '';
        };
      in {
        default = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              git
              git-crypt
            ]
            ++ builtins.attrValues scripts;
        };
      }
    );

    nixosConfigurations = (
      import ./hosts {
        inherit nixpkgs;
        system = "x86_64_linux";
        pkgs = pkgsFor.x86_64-linux;
      }
    );

    homeConfigurations = {
      "guilherme@linux" = home-manager.lib.homeManagerConfiguration {
        extraSpecialArgs = {
          inherit dream2nix secrets;
          system = linux;
          yafl = yafl.packages.x86_64-linux.default;
          stable = pkgsFor.x86_64-linux;
          edgePkgs = pkgsEdgeFor.x86_64-linux;
          user = "guilherme";
          homeDirectory = "/home/guilherme";
        };
        pkgs = pkgsUnstableFor.x86_64-linux;
        modules = [
          ./home.nix
          ./home-linux.nix
        ];
      };

      "guilherme@darwin" = home-manager.lib.homeManagerConfiguration {
        extraSpecialArgs = {
          inherit dream2nix secrets;
          system = mac;
          yafl = yafl.packages.aarch64-darwin.default;
          stable = pkgsFor.aarch64-darwin;
          edgePkgs = pkgsEdgeFor.aarch64-darwin;
          user = secrets.work.macUser;
          homeDirectory = "/Users/${secrets.work.macUser}";
        };
        pkgs = pkgsUnstableFor.aarch64-darwin;
        modules = [
          ./home.nix
          ./home-darwin.nix
        ];
      };
    };
  };
}
