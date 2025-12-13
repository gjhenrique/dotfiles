{
  pkgsFor,
  nixpkgs,
  dellNvidia,
}: {
  dell = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";

    specialArgs = {
      pkgs = pkgsFor."x86_64-linux";

      inherit dellNvidia;

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
