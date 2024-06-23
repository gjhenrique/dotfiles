{ pkgs, system, nixpkgs, hyprland }:
{
  vm = nixpkgs.lib.nixosSystem {
    inherit system;

    specialArgs = {
      inherit hyprland pkgs;

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
      inherit hyprland pkgs;

      host = {
        hostName = "desktop";
      };
    };
    modules = [
      ./desktop
      ./configuration.nix
    ];
  };
}
