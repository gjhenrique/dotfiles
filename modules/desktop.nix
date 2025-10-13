# Copied from https://github.com/Cody-W-Tucker/nix-config/blob/bc93a321127d40a5d857736c92fcc148c2859e61/modules/server/paperless-scanning.nix

{ config, lib, pkgs, ... }:

with lib;

{
  options = {

    services.desktop.enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable support for desktop packages
      '';
    };

  };

  config = mkIf config.services.desktop.enable {
    environment.systemPackages = with pkgs; [
      hyprland
      kitty # other terminal thank foot for initial bootstrap

      google-chrome
    ];

    fonts = {
      packages = with pkgs; [
        jetbrains-mono

        # Waybar
        font-awesome
        (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})

        noto-fonts
        noto-fonts-emoji

        roboto
        roboto-mono
      ];

      enableDefaultPackages = true;

      fontconfig.defaultFonts = {
        serif = ["Noto Serif" "Source Han Serif"];
        sansSerif = ["Jetbrains Mono"];
        emoji = ["Noto Color Emoji"];
      };
    };
    # Enable networking
    networking.networkmanager.enable = true;
    systemd.services.NetworkManager-wait-online.enable = false;

    programs._1password-gui.enable = true;
    programs._1password-gui.polkitPolicyOwners = ["guilherme"];
    programs._1password.enable = true;

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    programs.gnupg.agent = {
      enable = true;
      #   enableSSHSupport = true;
    };

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;

      wireplumber.extraConfig = {
        "50-bluez" = {
          "monitor.bluez.rules" = [
            {
              matches = [{"device.name" = "~bluez_card.*";}];
              actions = {
                update-props = {
                  "bluez5.auto-connect" = [
                    "a2dp_sink"
                    "a2dp_source"
                  ];
                  "bluez5.hw-volume" = [
                    "a2dp_sink"
                    "a2dp_source"
                  ];
                };
              };
            }
          ];
          "monitor.bluez.properties" = {
            "bluez5.roles" = [
              "a2dp_sink"
              "a2dp_source"
              "bap_sink"
              "bap_source"
            ];

            "bluez5.codecs" = [
              "ldac"
              "aptx"
              "aptx_ll_duplex"
              "aptx_ll"
              "aptx_hd"
              "opus_05_pro"
              "opus_05_71"
              "opus_05_51"
              "opus_05"
              "opus_05_duplex"
              "aac"
              "sbc_xq"
            ];

            "bluez5.hfphsp-backend" = "none";
          };
        };
      };

      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    services.xserver.enable = true;
    services.xserver = {
      xkbOptions = "caps:escape";
    };
    services.displayManager.sddm.enable = true;
    services.desktopManager.plasma6.enable = true;

    programs.firefox = {
      enable = true;
      package = pkgs.firefox;
    };

    programs.hyprland.enable = true;

    services.tailscale.enable = true;

    services.syncthing = {
      enable = true;
      user = "guilherme";
      configDir = "/home/guilherme/.config/syncthing";
    };
  };
}
