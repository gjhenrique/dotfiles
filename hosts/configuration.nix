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

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.guilherme = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = ["networkmanager" "wheel" "docker" "libvirtd"];
  };

  environment.variables = {
    EDITOR = "nvim";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # basic packages for servers and desktop
    gcc
    git
    htop
    neovim
  ];

  environment.localBinInPath = true;

  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  virtualisation = {
    docker = {
      package = pkgs.docker;
      enable = true;
      daemon.settings = {
        dns = [
          "172.17.0.1"
        ];
      };
    };
  };

  networking.firewall.enable = false;

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
