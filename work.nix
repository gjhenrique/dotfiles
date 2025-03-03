{
  dream2nix,
  config,
  edgePkgs,
  stable,
  pkgs,
  system,
  secrets,
  ...
}: let
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
in {
  home.file = {
    ".ssh/config".text = builtins.readFile ./secrets/ssh_config;

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

  home.sessionVariables = secrets.work.envs // {
    TERRAGRUNT_FETCH_DEPENDENCY_OUTPUT_FROM_STATE = "true";
  } // (if pkgs.stdenv.isLinux then {
    # use nixos pkgs because it comes from there
    PINENTRY_PROGRAM = "${stable.pinentry-gtk2}/bin/pinentry-gtk-2";
  } else {
    # don't use emacsclient in macos
    EDITOR = "nvim";
  });

  programs.zsh.initExtra = ''
    ${secrets.work.script}
  '';

  home.packages = with pkgs;
    [
      act
      amazon-ecr-credential-helper
      awscli2
      devbox
      dive
      fluxcd
      google-cloud-sdk
      helmfile
      jetbrains.gateway
      jetbrains.idea-community-bin
      (wrapHelm kubernetes-helm {
        plugins = with pkgs.kubernetes-helmPlugins; [
          helm-diff
          helm-git
          helm-s3
        ];
      })
      mysql-client
      onelogin-aws-assume-role
      src-cli
      stern
      ssm-session-manager-plugin
      teleport
      terragrunt
    ]
    ++ [
      edgePkgs.devpod
      edgePkgs.vscode
    ];

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
