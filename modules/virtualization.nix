# Copied from https://github.com/Cody-W-Tucker/nix-config/blob/bc93a321127d40a5d857736c92fcc148c2859e61/modules/server/paperless-scanning.nix

{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    services.virtualization.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Adds virtualization support
      '';
    };

  };

  config = mkIf config.services.virtualization.enable {
    virtualisation = {
      oci-containers = {
        backend = "docker";
        containers.registry = {
          image = "registry:2";
          ports = ["5001:5000"];
          volumes = ["/var/lib/registry:/var/lib/registry"];
        };
      };

      spiceUSBRedirection.enable = true;

      libvirtd = {
        enable = true;
        qemu = {
          package = pkgs.qemu_kvm;
          runAsRoot = true;
          swtpm.enable = true;
          ovmf = {
            enable = true;
          };
        };
      };
    };

    programs.virt-manager.enable = true;

    environment.systemPackages = with pkgs; [
      qemu
      virtiofsd # for sharing folder in libvirt
    ];
  };
}
