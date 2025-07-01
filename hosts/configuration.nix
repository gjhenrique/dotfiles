# Edit this configuration file to define what should be installed on # your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  host,
  config,
  pkgs,
  edgePkgs,
  ...
}: {
  boot.tmp.useTmpfs = true;
  boot.kernel.sysctl = {
    "fs.inotify.max_user_instances" = 4096;
    "fs.inotify.max_user_watches" = 524288;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];
  # required by devenv
  nix.settings.trusted-users = ["root" "guilherme"];

  networking.hostName = host.hostName; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.extraHosts = ''
    127.0.0.1 image-build-cache.registry.svc.cluster.local
  '';

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.xserver.enable = true;
  services.xserver = {
    xkbOptions = "caps:escape";
  };
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
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

  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.guilherme = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = ["networkmanager" "wheel" "docker" "libvirtd"];
  };

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

  environment.variables = {
    EDITOR = "emacsclient";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hyprland
    kitty

    # run buildkit locally
    runc

    google-chrome

    qemu

    virtiofsd # for sharing folder in libvirt
  ];

  environment.localBinInPath = true;

  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  virtualisation = {
    docker = {
      package = edgePkgs.docker;
      enable = true;
      daemon.settings = {
        dns = [
          "172.17.0.1"
        ];
      };
    };

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
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };
  };

  programs.virt-manager.enable = true;

  fonts = {
    packages = with pkgs; [
      jetbrains-mono

      # Waybar
      font-awesome
      (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})

      noto-fonts
      noto-fonts-emoji

      # for compiling some stuff
      gcc

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

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.

  # networking.firewall.enable = true;
  networking.firewall.enable = false;

  # allow docker to query dns with systemd-resolved
  networking.firewall.trustedInterfaces = [
    # default docker
    "docker0"
    # libvirt
    "virbr0"
    # generated kind brige interface
    "br-5d5b9af4af7f"
    "br-1e56f9cdce85"
  ];

  networking.nameservers = ["1.1.1.1#one.one.one.one" "1.0.0.1#one.one.one.one"];
  services.resolved = {
    enable = true;
    domains = ["~."];
    fallbackDns = ["1.1.1.1#one.one.one.one" "1.0.0.1#one.one.one.one"];
    extraConfig = ''
      # docker default bridge
      DNSStubListenerExtra=172.17.0.1
      # kind default bridge
      DNSStubListenerExtra=172.18.0.1
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
