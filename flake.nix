{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = {
    home-manager,
    nixpkgs,
    nixpkgs-unstable,
    dream2nix,
    hyprland,
    agenix,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    username = "guilherme";

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    pkgs-unstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.${system}.alejandra;

    nixosConfigurations = (
      import ./hosts {
        inherit pkgs system nixpkgs hyprland;
      }
    );

    homeConfigurations = {
      guilherme = home-manager.lib.homeManagerConfiguration {
        extraSpecialArgs = {
          inherit dream2nix system agenix;
        };
        pkgs = pkgs-unstable;
        modules = [
          ./home.nix

          agenix.homeManagerModules.age
          {
            age.secrets.work.file = ./secrets/work.age;
          }
        ];
      };
    };
  };
}
