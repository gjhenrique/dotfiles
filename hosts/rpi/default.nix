{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  users.users.guilherme.shell = pkgs.bash;

  services.ssh-auth.enable = true;

    fileSystems."/mnt/external" = {
    device = "/dev/sda1";
    fsType = "ext4";
    options = [ "nofail" ];
  };
}
