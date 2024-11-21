{
  pkgs,
  system,
  nixpkgs,
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
    inherit system;

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
      inherit pkgs;

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
