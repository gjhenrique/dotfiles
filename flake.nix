{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

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
    self,
    yafl,
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

    secrets = builtins.fromJSON (builtins.readFile "${self}/secrets/secrets.json");
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.${system}.alejandra;

    nixosConfigurations = (
      import ./hosts {
        inherit pkgs system nixpkgs;
      }
    );

    homeConfigurations = {
      guilherme = home-manager.lib.homeManagerConfiguration {
        extraSpecialArgs = {
          inherit dream2nix system secrets;
          yafl = yafl.packages.${system}.default;
          stable = pkgs;
        };
        pkgs = pkgs-unstable;
        modules = [
          ./home.nix
        ];
      };
    };
  };
}
