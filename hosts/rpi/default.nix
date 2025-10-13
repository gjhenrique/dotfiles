{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  services.openssh = {
  enable = true;
  ports = [ 2201 ];
  settings = {
    PasswordAuthentication = true;
    AllowUsers = [ "guilherme" ];
    UseDns = true;
    X11Forwarding = false;
    PermitRootLogin = "prohibit-password";
    };
  };
}
