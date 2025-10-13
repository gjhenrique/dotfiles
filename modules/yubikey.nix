# Copied from https://github.com/Cody-W-Tucker/nix-config/blob/bc93a321127d40a5d857736c92fcc148c2859e61/modules/server/paperless-scanning.nix

{ config, lib, pkgs, ... }:

with lib;

{

  ###### interface
  options = {

    services.yubikey.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Adds support for authentication with yubikey
      '';
    };

  };

  ###### implementation
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
