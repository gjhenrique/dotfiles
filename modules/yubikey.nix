{ config, lib, pkgs, ... }:

with lib;

{
  options = {

    services.yubikey.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Adds support for authentication with yubikey
      '';
    };

  };

  config = mkIf config.services.yubikey.enable {
    # https://nixos.wiki/wiki/Yubikey
    services.pcscd.enable = true;
    security.polkit.enable = true;
    security.polkit.debug = true;
    services.udev.packages = [pkgs.yubikey-personalization];
    security.pam.yubico = {
      enable = true;
      # debug = true;
      mode = "challenge-response";
      id = ["22433541"];
    };
  };
}
