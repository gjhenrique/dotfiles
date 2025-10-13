{
  pkgs,
  system,
  nixpkgs,
  edgePkgs,
  dellNvidia
}: {
  lisa = nixpkgs.lib.nixosSystem {
    system = "x86_64_linux";

    specialArgs = {
      inherit pkgs edgePkgs;

      host = {
        hostName = "lisa";
      };
    };
    modules = [
      ./lisa
      ./configuration.nix
      ../modules/scanning.nix
    ];
  };
  rpi = nixpkgs.lib.nixosSystem {
    system = "aarch64_linux";

    specialArgs = {
      inherit pkgs edgePkgs;

      host = {
        hostName = "rpi";
      };
    };
    modules = [
      ./rpi
      ./configuration.nix
    ];
  };
  dell = nixpkgs.lib.nixosSystem {
    system = "x86_64_linux";

    specialArgs = {
      inherit pkgs edgePkgs dellNvidia;

      host = {
        hostName = "dell";
      };
    };
    modules = [
      ./dell
      ./configuration.nix
      dellNvidia
      ../modules/scanning.nix
      ../modules/desktop.nix
      ../modules/virtualization.nix
      ../modules/yubikey.nix
    ];
  };
}
