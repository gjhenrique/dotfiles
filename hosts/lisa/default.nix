{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  users.users.guilherme.shell = pkgs.zsh;

  services.openssh = {
  enable = true;
  ports = [ 2200 ];
  settings = {
    PasswordAuthentication = false;
    AllowUsers = [ "guilherme" ];
    UseDns = true;
    X11Forwarding = false;
    PermitRootLogin = "prohibit-password";
    };
  };

  # custom modules
  services.scanbd.enable = true;
  services.scanbd.user = "guilherme";
  services.ssh-auth.enable = true;
}
