{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

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

  inputs.nixpkgs-edge.url = "github:nixos/nixpkgs/nixos-unstable";

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

    pkgs-edge = import nixpkgs-edge {
      inherit system;
      config.allowUnfree = true;
    };

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
          inherit dream2nix system;
          yafl = yafl.packages.${system}.default;
          stable = pkgs;
          edgePkgs = pkgs-edge;
        };
        pkgs = pkgs-unstable;
        modules = [
          ./home.nix
        ];
      };
    };
  };
}
