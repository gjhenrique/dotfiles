{
  pkgs,
  system,
  nixpkgs,
  edgePkgs
}: {
  vm = nixpkgs.lib.nixosSystem {
    inherit system;

    specialArgs = {
      inherit pkgs;

      host = {
        hostName = "vm";
      };
    };
    modules = [
      ./vm
      ./configuration.nix
    ];
  };
  desktop = nixpkgs.lib.nixosSystem {
    inherit pkgs edgePkgs;

    specialArgs = {
      inherit pkgs;

      host = {
        hostName = "desktop";
      };
    };
    modules = [
      ./desktop
      ./configuration.nix
    ];
  };
  dell = nixpkgs.lib.nixosSystem {
    inherit system;

    specialArgs = {
      inherit pkgs edgePkgs;

      host = {
        hostName = "dell";
      };
    };
    modules = [
      ./dell
      ./configuration.nix
    ];
  };
}
