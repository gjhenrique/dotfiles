{
  pkgs,
  nixpkgs,
  dellNvidia,
}: {
  dell = nixpkgs.lib.nixosSystem {
    specialArgs = {
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
      nixpkgs.nixosModules.readOnlyPkgs
      {nixpkgs.pkgs = pkgs;}
    ];
  };
}
