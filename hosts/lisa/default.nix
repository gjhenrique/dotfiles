{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.guilherme.shell = pkgs.zsh;

   # custom modules
  services.scanbd.enable = true;
  services.scanbd.user = "guilherme";
  services.ssh-auth.enable = true;
}
