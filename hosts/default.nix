{
  pkgs,
  nixpkgs,
  dellNvidia,
  system,
}: {
  dell = nixpkgs.lib.nixosSystem {
    inherit system;

    specialArgs = {
      inherit pkgs dellNvidia;

      host = {
        hostName = "dell";
      };
    };
    modules = [
      ./dell
      ./configuration.nix
      dellNvidia
      ../modules/desktop.nix
      ../modules/virtualization.nix
      ../modules/yubikey.nix
    ];
  };
}
