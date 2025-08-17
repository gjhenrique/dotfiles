{
  pkgs,
  system,
  nixpkgs,
  edgePkgs,
  dellNvidia
}: {
  lisa = nixpkgs.lib.nixosSystem {
    inherit system;

    specialArgs = {
      inherit pkgs edgePkgs;

      host = {
        hostName = "lisa";
      };
    };
    modules = [
      ./lisa
      ./configuration.nix
    ];
  };
  dell = nixpkgs.lib.nixosSystem {
    inherit system;

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
    ];
  };
}
