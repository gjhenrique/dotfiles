{ dream2nix, config, edgePkgs, pkgs, system, ... }:

let
  onelogin-aws-assume-role = dream2nix.lib.evalModules {
    packageSets.nixpkgs = pkgs;
    modules = [
      ./onelogin-aws-assume-role.nix
      {
        paths.projectRoot = ./.;
        paths.projectRootFile = ".git";
        paths.package = ./.;
      }
    ];
  };

  secrets = builtins.fromJSON (builtins.readFile ./secrets/secrets.json);
in {
  home.file = {
    ".ssh/config".text = builtins.readFile ./secrets/ssh_config;

    ".ssh/config.overrides".text = builtins.readFile ./secrets/ssh_overrides;

    ".prezi.gitconfig".text = ''
    [user]
      email = ${secrets.work.email}
      name = ${secrets.work.githubUsername}
    '';

    ".local/bin/aws-login" = {
      text = ''
      #!/usr/bin/env bash

      ${secrets.work.oneloginScript}
      '';
      executable = true;
      target = ".local/bin/aws-login";
    };
  };

  home.sessionVariables = secrets.work.envs;

  programs.zsh.initExtra = ''
    ${secrets.work.script}
  '';

  home.packages = with pkgs; [
    _1password-gui
    amazon-ecr-credential-helper
    ansible
    awscli2
    devbox
    dive
    jetbrains.gateway
    jetbrains.idea-community-bin
    onelogin-aws-assume-role
    slack
    src-cli
    stern
    spotify
    teleport
  ] ++ [
    edgePkgs.aider-chat
    edgePkgs.devpod
    edgePkgs.vscode
  ];


  xdg.dataFile."applications/slack-wayland.desktop".text = pkgs.lib.generators.toINI {} {
    "Desktop Entry" = {
      Name = "Slack (Wayland)";
      StartupWMClass = "Slack";
      Comment = "Slack Desktop (Custom)";
      GenericName = "Slack Client for Linux";
      Exec = "${pkgs.slack}/bin/slack --ozone-platform=wayland --enable-features=WaylandWindowDecorations,UseOzonePlatform,WebRTCPipeWireCapturer";
      Icon = "slack";
      Type = "Application";
      StartupNotify = "true";
      Categories = "GNOME;GTK;Network;InstantMessaging;";
      MimeType = "x-scheme-handler/slack;";
    };
  };


  programs.git = {
    includes = [
      {
        path = "${config.home.homeDirectory}/.prezi.gitconfig";
        condition = "gitdir:~/.prezi/";
      }
      {
        path = "${config.home.homeDirectory}/.prezi.gitconfig";
        condition = "gitdir:~/Projects/prezi/";
      }
    ];
  };
}
