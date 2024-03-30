{ pkgs, system, nixpkgs, hyprland }:
{
  nixos = nixpkgs.lib.nixosSystem {
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
