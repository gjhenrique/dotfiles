{ config, pkgs, ... }: let
  tpm = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tpm";
    version = "2023-08-04";
    src = pkgs.fetchFromGitHub {
      owner = "tmux-plugins";
      repo = "tpm";
      rev = "99469c4a9b1ccf77fade25842dc7bafbc8ce9946";
      sha256 = "sha256-hW8mfwB8F9ZkTQ72WQp/1fy8KL1IIYMZBtZYIwZdMQc=";
    };
  };
in {
  home.username = "guilherme";
  home.homeDirectory = "/home/guilherme";

  home.stateVersion = "23.11"; # Please read the comment before changing.

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    _1password
    awscli2
    bat
    devbox
    gh
    jq
    kubie
    kubectl
    kubectx
    foot
    neovim
    ripgrep
    theme-sh
  ];

  # WTF is this?
  # TODO: Make this use home.file and point to the correct symlink
  # Flakes doesn't work with mkOutOfStoreSymlink
  home.activation.linkMyFiles = config.lib.dag.entryAfter ["writeBoundary"] ''
    ln -s ~/Projects/mine/dotfiles/nix/zezin.emacs ~/.emacs.d
  '';

  home.file = {
    "switch_theme" = {
      source = ./scripts/switch_theme;
      target = ".local/bin/switch_theme";
    };

    "yafl_ext" = {
      source = ./scripts/yafl_ext;
      target = ".local/bin/yafl_ext";
    };

    "rgconfig" = {
      target = ".rgconfig";
      text = ''
        --hidden
        --glob=!.git/*
      '';
    };

    "gitignore_global" = {
      text = ''
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
      target = ".config/git/gitignore";
    };
  };

  home.sessionVariables = {
    EDITOR = "emacsclient";
    PATH = "$HOME/.local/bin:$PATH";
    DNS_RESOLVER = "systemd-resolved";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

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

  programs.gh = {
    enable = true;
    settings = {
      editor = "emacsclient";
    };
  };

  programs.tmux = {
    enable = true;
    sensibleOnTop = false;
    extraConfig = builtins.readFile ./tmux.conf;

    plugins = with pkgs; [
      # TODO: Fix this
      # {
      #   plugin = tpm;
      # }
    ];
  };

  programs.foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        font= "JetBrainsMono NF:size=8";
      };
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    shellAliases = {
      pg = "ping google.com";
      images = "kubectl get pods --all-namespaces -o jsonpath=\"{.items[*].spec.containers[*].image}\" |tr -s '[[:space:]]' '\n' |sort |uniq -c";
      pods-image = "kubectl get pods -o wide --sort-by=.spec.nodeName";
      xc="wl-copy";
      xco="wl-paste";
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
        golang = "1.20";
        java = "11";
        gradle = "8.2";
        python = "3.10";
        ruby = "3.2";
        terraform = "1.5.4";
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
    /home/guilherme/Life/work/work.nix
    # (import ./work.nix { inherit pkgs config; })
  ];
}
