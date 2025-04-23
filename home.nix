{
  config,
  dream2nix,
  edgePkgs,
  pkgs,
  secrets,
  stable,
  system,
  user,
  homeDirectory,
  yafl,
  ...
}: {
  home.username = user;
  home.homeDirectory = homeDirectory;
  home.stateVersion = "23.11"; # Please read the comment before changing.

  home.sessionPath = [
    "$HOME/.local/bin:$PATH"
  ];

  nixpkgs.config.allowUnfree = true;

  programs.home-manager.enable = true;

  home.packages = with pkgs;
    [
      # languages
      go
      gradle
      jdk11
      nodejs_18
      python311
      python311Packages.pip
      # python310Packages.requests
      virtualenv
      ruby
      opentofu
      terraform
      uv

      # language servers
      terraform-ls
      gopls

      # security
      age-plugin-yubikey
      passage
      rage

      # command-line
      android-tools
      bat
      bc
      edgePkgs.curl
      delve
      dogdns
      file
      gh
      git
      git-filter-repo
      gnumake
      htop
      jq
      killall
      kubectl
      kubectx
      kubie
      meld
      ncdu
      neovim
      openssl
      pciutils
      ripgrep
      tcpdump
      theme-sh
      tree
      unp
      wget
      whois
      wl-clipboard
      xdg-utils
      yafl
      yq
    ]
    ++ [
      edgePkgs.aider-chat
    ];

  # WTF is this?
  # TODO: Make this use home.file and point to the correct symlink
  # Flakes doesn't work with mkOutOfStoreSymlink
  home.activation.linkMyFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -sf ${config.home.homeDirectory}/Projects/mine/dotfiles/zezin.emacs/ ${config.home.homeDirectory}/.emacs.d
  '';

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

  xdg.configFile."yafl/config.toml".text = builtins.readFile ./files/yafl-config.toml;
  xdg.configFile."yafl/search.json".text = builtins.readFile ./files/yafl-search.json;

  xdg.configFile."pipewire/pipewire.conf.d/echo-cancel.conf".text = ''
    context.modules = [{ name = libpipewire-module-echo-cancel }]
  '';

  # Scripts
  home.file = {
    "phinger-cursors-dark" = {
      source = "${pkgs.phinger-cursors}/share/icons/phinger-cursors-dark";
      target = ".local/share/icons/phinger-cursors-dark";
    };

    "switch_theme" = {
      source = ./files/switch_theme;
      target = ".local/bin/switch_theme";
    };

    "pfzf" = {
      source = ./files/passage-fzf;
      target = ".local/bin/pfzf";
      executable = true;
    };

    "yafl_ext" = {
      source = ./files/yafl_ext;
      target = ".local/bin/yafl_ext";
    };
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
      advice.skippedCherryPicks = false;

      core = {
        editor = "emacsclient";
        excludesfile = "~/.config/git/gitignore";
      };

      github = {
        user = "gjhenrique";
      };
    };
  };
  xdg.configFile."git/gitignore".text = ''
    .dir-locals.el

    # some apps don't like .devcontainer
    .devcontainer

    venv
    virtualenv

    # lsp-mode
    .log
    .tool-versions

    # direnv stuff
    .envrc
    .direnv

    # aider
    .aider*
    .env

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

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    shellAliases = {
      pg = "ping google.com";
      images = "kubectl get pods --all-namespaces -o jsonpath=\"{.items[*].spec.containers[*].image}\" |tr -s '[[:space:]]' '\n' |sort |uniq -c";
      pods-image = "kubectl get pods -o wide --sort-by=.spec.nodeName";
      xc = "wl-copy";
      xco = "wl-paste";
      # more powerful editing capabilities
      ssi = "grim -g \"$(slurp -d)\" - | swappy -f -";
    };
    oh-my-zsh = {
      enable = true;
      plugins = ["git" "systemd" "autojump" "aws" "kubectl"];
    };

    envExtra = ''
      if [ -f /etc/set-environment ]; then
        source /etc/set-environment
      fi
    '';

    initExtra = ''
      # nixos variables
      if [ -e /etc/profile.d/nix-daemon.sh  ]; then . /etc/profile.d/nix-daemon.sh; fi

      if [ "$TERM" != "linux" ] && [ "$TERM" != "dumb" ]
      then
        # Start shell with tmux
        # If not running interactively, do not do anything
        [[ $- != *i* ]] && return
        [[ -z "$TMUX" ]] && TERM=xterm-256color exec ${pkgs.tmux}/bin/tmux -2
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
  };

  programs.wezterm = {
    enable = true;
    extraConfig = builtins.readFile ./files/wezterm.lua;
  };

  programs.starship = {
    enable = true;
    settings = {
      add_newline = true;

      kubernetes = {
        disabled = false;
        format = "on [⛵ $context \($namespace\)](dimmed green) ";
      };
    };
  };

  programs.k9s.enable = true;

  programs.atuin = {
    enable = true;
    settings = {
      inline_height = 30;
      style = "compact";
      search_mode = "fulltext";
    };
  };

  programs.autojump.enable = true;

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  imports = [
    (import ./work.nix {inherit pkgs edgePkgs config dream2nix system secrets stable;})
  ];
}
