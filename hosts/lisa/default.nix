{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.openssh = {
  enable = true;
  ports = [ 2200 ];
  settings = {
    PasswordAuthentication = true;
    AllowUsers = [ "guilherme" ];
    UseDns = true;
    X11Forwarding = false;
    PermitRootLogin = "prohibit-password";
    };
  };

  services.scanbd = {
    enable = true;
    user = "guilherme";
  };

  #boot.initrd.luks.devices."luks-5710ad9f-67e9-4959-b39c-2a671716bef3".device = "/dev/disk/by-uuid/5710ad9f-67e9-4959-b39c-2a671716bef3";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
}
