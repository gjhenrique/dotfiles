{
  config,
  pkgs,
  ...
}: {
  home.username = "guilherme";
  home.homeDirectory = "/home/guilherme";

  home.stateVersion = "23.11"; # Please read the comment before changing.

  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    _1password
    awscli2
    bat
    devbox
    gh
    nodePackages.js-beautify
    jq
    kubie
    kubectl
    kubectx
    foot
    neovim
    ripgrep
    theme-sh
    (pkgs.callPackage ./yafl.nix {inherit pkgs;})
  ];

  # WTF is this?
  # TODO: Make this use home.file and point to the correct symlink
  # Flakes doesn't work with mkOutOfStoreSymlink
  home.activation.linkMyFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${config.home.homeDirectory}/Projects/mine/dotfiles.git/main/zezin.emacs/ ${config.home.homeDirectory}/.emacs.d
  '';

  services.mako = {
    enable = true;

    defaultTimeout = 5000;
    width = 300;
    height = 200;
    padding = "20";
    margin = "20";
    font = "JetBrainsMono NF 14";
    backgroundColor = "#24273a";
    borderColor = "#8aadf4";
    textColor = "#cad3f5";

    extraConfig = ''
      [urgency=high]
      border-color=#f5a97f

      [mode=do-not-disturb]
      invisible=1
    '';
  };

  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";

    profiles = {
      docked = {
        outputs = [
          {
            criteria = "Dell Inc. DELL U2715H GH85D71G1R9S";
            scale = 1.33333333;
            transform = "270";
            position = "0,0";
            mode = "2560x1440";
          }
          {
            criteria = "LG Electronics LG HDR 4K 0x00067273";
            scale = 2.0;
            position = "1080,450";
            mode = "3840x2160";
          }
          {
            criteria = "Dell Inc. DELL S2319HS 95FVKS2";
            transform = "90";
            scale = 1.2;
            position = "3000,0";
            mode = "1920x1080";
          }
          {
            criteria = "eDP-1";
            status = "disable";
          }
        ];
      };
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      };
      docked_room = {
        outputs = [
          {
            criteria = "BNQ BenQ EW3270U G9K02925019";
            mode = "3840x2160";
            position = "0,0";
            scale = 2.0;
          }
          {
            criteria = "eDP-1";
            position = "1930,0";
            scale = 2.0;
          }
        ];
      };
    };
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;

    style = builtins.readFile ./files/waybar-style.css;
    settings = [
      {
        position = "top";
        height = 30;
        modules-left = ["hyprland/workspaces"];
        modules-center = ["clock"];
        modules-right = ["cpu" "memory" "pulseaudio" "battery"];

        "hyprland/workspace" = {
          "disable-scroll" = true;
        };

        clock = {
          format = "󰸗 {:%d.%m - %H:%M}";
          interval = 1;
          tooltip-format = "<big>{:%B %Y}</big>\n<tt>{calendar}</tt>";
          on-click = "hyprctl dispatch exec xdg-open https://calendar.google.com";
        };

        cpu = {
          format = "{usage}% ";
          interval = 1;
        };

        memory = {
          format = "{percentage}% 󰍛";
          interval = 1;
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = "ﱝ {icon} {format_source}";
          format-muted = "ﱝ";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphones = "";
            handsfree = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
        };

        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon}";
          format-charging = "{capacity}% 󰂅";
          format-icons = ["󰁹" "󰂂" "󰂁" "󰂀" "󰁿" "󰁾" "󰁽" "󰁼" "󰁻" "󰁺"];
          tooltip-format = "{time}";
        };
      }
    ];
  };

  xdg.enable = true;
  xdg.dataFile."applications/emacs-setup.desktop".text = pkgs.lib.generators.toINI {} {
    "Desktop Entry" = {
      Name = "Emacs Setup";
      GenericName = "Text Editor";
      Comment = "Spawn specific Emacs instances";
      MimeType = "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;";
      Exec = "emacsclient -c -e \"(zezin-start-frames)\"";
      Icon = "emacs";
      Type = "Application";
      Terminal = "false";
      Categories = "Development;TextEditor;";
      StartupWMClass = "Emacs";
      Keywords = "Text;Editor;";
    };
  };

  xdg.dataFile."applications/slack-wayland.desktop".text = pkgs.lib.generators.toINI {} {
    "Desktop Entry" = {
      Name = "Slack (Wayland)";
      StartupWMClass = "Slack";
      Comment = "Slack Desktop (Custom)";
      GenericName = "Slack Client for Linux";
      Exec = "/bin/slack --ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer";
      Icon = "slack";
      Type = "Application";
      StartupNotify = "true";
      Categories = "GNOME;GTK;Network;InstantMessaging;";
      MimeType = "x-scheme-handler/slack;";
    };
  };

  # Move this to .profile when I move away from manjaro-sway
  xdg.configFile."profile.d/01-path".text = ''
    export PATH="$HOME/.nix-profile/bin:$HOME/.local/bin:$PATH"
  '';

  xdg.configFile."yafl/config.toml".text = builtins.readFile ./files/yafl-config.toml;
  xdg.configFile."yafl/search.json".text = builtins.readFile ./files/yafl-search.json;

  # Scripts
  home.file = {
    "switch_theme" = {
      source = ./files/switch_theme;
      target = ".local/bin/switch_theme";
    };

    "yafl_ext" = {
      source = ./files/yafl_ext;
      target = ".local/bin/yafl_ext";
    };
  };

  home.sessionVariables = {
    EDITOR = "emacsclient";
    PATH = "$HOME/.local/bin:$PATH";
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "gjhenrique";
    userEmail = "me@gjhenrique.com";

    extraConfig = {
      core = {
        editor = "emacsclient";
        excludesfile = "~/.config/git/gitignore";
      };
    };
  };
  xdg.configFile."git/gitignore".text = ''
    .dir-locals.el

    virtualenv

    # lsp-mode
    .log
    .tool-versions

    # direnv stuff
    .envrc

    # emacs
    *~
    \#*\#
  '';

  programs.gh = {
    enable = true;
    settings = {
      editor = "emacsclient";
    };
  };

  home.file."tpm" = {
    source = pkgs.fetchFromGitHub {
      owner = "tmux-plugins";
      repo = "tpm";
      rev = "99469c4a9b1ccf77fade25842dc7bafbc8ce9946";
      hash = "sha256-hW8mfwB8F9ZkTQ72WQp/1fy8KL1IIYMZBtZYIwZdMQc=";
    };
    target = ".tmux/plugins/tpm";
  };

  programs.tmux = {
    enable = true;
    sensibleOnTop = false;
    extraConfig = builtins.readFile ./files/tmux.conf;
  };

  # TODO: Move this to hyprland module once NixOS is in place
  xdg.configFile."hypr/hyprland.conf".text = builtins.readFile ./files/hyprland.conf;
  systemd.user.targets.hyprland-session = {
    Unit = {
      Description = "Hyprland compositor session";
      Documentation = ["man:systemd.special(7)"];
      BindsTo = ["graphical-session.target"];
      Wants = ["graphical-session-pre.target"];
      After = ["graphical-session-pre.target"];
    };
  };
  # wayland.windowManager.hyprland = {
  #   enable = true;
  # };

  programs.foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        font = "JetBrainsMono NF:size=10";
      };
    };
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    shellAliases = {
      pg = "ping google.com";
      images = "kubectl get pods --all-namespaces -o jsonpath=\"{.items[*].spec.containers[*].image}\" |tr -s '[[:space:]]' '\n' |sort |uniq -c";
      pods-image = "kubectl get pods -o wide --sort-by=.spec.nodeName";
      xc = "wl-copy";
      xco = "wl-paste";
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["git" "systemd" "autojump" "aws" "kubectl"];
    };
  };

  programs.starship.enable = true;

  programs.k9s.enable = true;

  programs.atuin = {
    enable = true;
    settings = {
      inline_height = 30;
      style = "compact";
      search_mode = "fulltext";
      history_filter = ["^export"];
    };
  };

  programs.autojump.enable = true;

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.mise = {
    enable = true;
    enableZshIntegration = true;
    globalConfig = {
      tools = {
        node = "16";
        golang = "1.22";
        java = "11";
        gradle = "8.2";
        python = "3.10";
        ruby = "3.2";
        terraform = "1.7.5";
      };
    };
  };

  programs.zsh.initExtra = ''
    if [ -e /etc/profile.d/nix-daemon.sh  ]; then . /etc/profile.d/nix-daemon.sh; fi

    if [ "$TERM" != "linux" ] && [ "$TERM" != "dumb" ]
    then
      # Start shell with tmux
      # If not running interactively, do not do anything
      [[ $- != *i* ]] && return
      [[ -z "$TMUX" ]] && TERM=xterm-256color exec tmux -2
    fi

    DARK_THEME=dracula
    LIGHT_THEME=fruit-soda

    CURRENT_THEME=$(grep -q light $XDG_RUNTIME_DIR/theme 2>/dev/null && echo $LIGHT_THEME || echo $DARK_THEME)

    [[ $- == *i* ]] && TMUX= theme.sh $CURRENT_THEME

    # Based on https://codeberg.org/dnkl/foot/wiki#dynamic-color-changes
    TRAPUSR1() {
    TMUX= theme.sh $DARK_THEME
    }

    TRAPUSR2() {
    TMUX= theme.sh $LIGHT_THEME
    }
  '';

  # TODO: Don't import file if it doesn't exist
  imports = [
    # Needs --impure. How to keep outside of repo, but pure?
    # /home/guilherme/Life/work/work.nix
    # (import ./work.nix { inherit pkgs config; })
  ];
}
