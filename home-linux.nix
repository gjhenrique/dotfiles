{
  config,
  lib,
  pkgs,
  edgePkgs,
  ...
}: let
  sshOverrides = builtins.readFile ./secrets/ssh_overrides;
in {
  home.file = {
    ".ssh/config.overrides".text = builtins.replaceStrings ["@1p-agent@"] ["~/.1password/agent.sock"] sshOverrides;
  };

  # Prevent Zoom from creating its mini meeting window when its main window
  # becomes hidden after switching workspaces or monitors.
  home.activation.disableZoomMiniWindow = config.lib.dag.entryAfter ["writeBoundary"] ''
    zoom_config="$HOME/.config/zoomus.conf"
    if [[ -f "$zoom_config" ]]; then
      ${pkgs.gnused}/bin/sed -i 's/^enableMiniWindow=.*/enableMiniWindow=false/' "$zoom_config"
    fi
  '';

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      cursor-theme = config.home.pointerCursor.name;
      cursor-size = config.home.pointerCursor.size;
    };

    "org/gnome/desktop/input-sources" = {
      sources = [(lib.hm.gvariant.mkTuple ["xkb" "us+altgr-intl"])];
      xkb-options = ["caps:swapescape"];
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = ["<Super>q"];
      switch-to-workspace-1 = ["<Super>1"];
      switch-to-workspace-2 = ["<Super>2"];
      switch-to-workspace-3 = ["<Super>3"];
      switch-to-workspace-4 = ["<Super>4"];
      switch-to-workspace-5 = ["<Super>5"];
      switch-to-workspace-6 = ["<Super>6"];
      switch-to-workspace-7 = ["<Super>7"];
      switch-to-workspace-8 = ["<Super>8"];
      switch-to-workspace-9 = ["<Super>9"];
      move-to-workspace-1 = ["<Super><Shift>1"];
      move-to-workspace-2 = ["<Super><Shift>2"];
      move-to-workspace-3 = ["<Super><Shift>3"];
      move-to-workspace-4 = ["<Super><Shift>4"];
      move-to-workspace-5 = ["<Super><Shift>5"];
      move-to-workspace-6 = ["<Super><Shift>6"];
      move-to-workspace-7 = ["<Super><Shift>7"];
      move-to-workspace-8 = ["<Super><Shift>8"];
      move-to-workspace-9 = ["<Super><Shift>9"];
    };

    "org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = [
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal/"
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/launcher/"
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/web-search/"
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/bookmark/"
    ];

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/terminal" = {
      name = "Open a Terminal: foot";
      command = "foot";
      binding = "<Super>t";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/launcher" = {
      name = "Run an Application: wofi";
      command = "wofi --show drun";
      binding = "<Super>d";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/web-search" = {
      name = "Web Search";
      command = ''sh -c 'launcher_ext search "$(launcher_ext search | wofi --dmenu --prompt "Web Search")"' '';
      binding = "<Super>s";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/bookmark" = {
      name = "Open Bookmark";
      command = ''sh -c 'launcher_ext bookmark_open "$(launcher_ext bookmark_open | wofi --dmenu --prompt Bookmarks | cut -f1)"' '';
      binding = "<Super>b";
    };

    # Free Super+1..9 from GNOME's application-launch shortcuts for PaperWM workspaces.
    "org/gnome/shell/keybindings" = {
      switch-to-application-1 = [];
      switch-to-application-2 = [];
      switch-to-application-3 = [];
      switch-to-application-4 = [];
      switch-to-application-5 = [];
      switch-to-application-6 = [];
      switch-to-application-7 = [];
      switch-to-application-8 = [];
      switch-to-application-9 = [];
    };

    "org/gnome/shell".enabled-extensions = [
      pkgs.gnomeExtensions.paperwm.extensionUuid
    ];

    # Center new regular windows globally (not just Wofi).
    "org/gnome/mutter".center-new-windows = true;

    "org/gnome/shell/extensions/paperwm".winprops = [
      (builtins.toJSON {
        wm_class = "wofi";
        scratch_layer = true;
      })
    ];

    "org/gnome/shell/extensions/paperwm/keybindings" = {
      switch-monitor-left = ["<Super>h"];
      switch-monitor-right = ["<Super>l"];
      move-space-monitor-right = ["<Super><Shift>l"];
      move-space-monitor-left = ["<Super><Shift>h"];
    };
  };

  programs.wofi = {
    enable = true;
    settings = {
      # GNOME lacks layer-shell; PaperWM floats this regular Wayland window.
      normal_window = true;
      width = 600;
      height = 400;
    };
  };

  home.packages = with pkgs; [
    gnomeExtensions.paperwm
    #desktop specific
    alacritty
    alsa-utils
    pinta
    kdePackages.okular

    # tools
    bluetuith
    brightnessctl
    distrobox
    emacs-pgtk
    git-crypt
    iotop
    # for compiling tree-sitter
    libgcc
    mpv
    qovery-cli
    runc
    pwvucontrol
    wl-clipboard

    # cli
    nftables
    js-beautify
    playerctl
    usbutils
  ];

  programs.foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        font = "JetBrainsMono NF:size=13";
      };
    };
  };

  gtk.enable = true;

  home.pointerCursor = {
    enable = true;
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry-gnome3;
  };

  imports = [
    (import ./work-linux.nix {inherit pkgs edgePkgs;})
  ];
}
