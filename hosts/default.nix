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
}
