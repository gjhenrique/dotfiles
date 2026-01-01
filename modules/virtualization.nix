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
    };

    programs.virt-manager.enable = true;

    environment.systemPackages = with pkgs; [
      qemu
      virtiofsd # for sharing folder in libvirt
    ];
  };
}
