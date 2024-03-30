{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  inputs.yafl = {
    url = "github:gjhenrique/yafl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    home-manager,
    nixpkgs,
    yafl,
    nixpkgs-unstable
    hyprland,
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
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;

    nixosConfigurations = (
      import ./hosts {
        inherit pkgs system nixpkgs hyprland;
      }
    );

    homeConfigurations = {
      guilherme = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgs-unstable;
        modules = [./home.nix];

        extraSpecialArgs = {
          yafl = yafl.packages.${system}.default;
        };
      };
    };
  };
}
