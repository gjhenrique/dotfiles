{
  pkgs,
  nixpkgs,
  secrets,
}: {
  dell = nixpkgs.lib.nixosSystem {
    specialArgs = {
      inherit secrets;
      host = {
        hostName = "dell";
      };
    };
    modules = [
      ./dell
      ./configuration.nix
      ../modules/desktop.nix
      ../modules/virtualization.nix
      ../modules/yubikey.nix
      nixpkgs.nixosModules.readOnlyPkgs
      {nixpkgs.pkgs = pkgs;}
    ];
  };
}
