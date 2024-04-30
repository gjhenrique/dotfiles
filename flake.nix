{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  inputs.yafl = {
    url = "github:gjhenrique/yafl";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    home-manager,
    nixpkgs,
    yafl,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    username = "guilherme";

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;

    homeConfigurations = {
      guilherme = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [./home.nix];

        extraSpecialArgs = {
          yafl = yafl.packages.${system}.default;
        };
      };
    };
  };
}
