{
  pkgsFor,
  nixpkgs,
  dellNvidia
}: {
  lisa = nixpkgs.lib.nixosSystem {
    system = "x86_64_linux";

    specialArgs = {
      pkgs = pkgsFor."x86_64-linux";

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
      pkgs = pkgsFor."aarch64-linux";

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
      ../modules/scanning.nix
      ../modules/desktop.nix
      ../modules/virtualization.nix
      ../modules/yubikey.nix
    ];
  };
}
