{ dream2nix, config, pkgs, secrets, system, ... }:

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
in {
  home.file = {
    ".prezi.gitconfig".text = ''
    [user]
      email = ${secrets.work.email}
      name = ${secrets.work.githubUsername}
    '';

    ".local/bin/aws-login" = {
      text = ''
      #!/bin/bash

      ${secrets.work.oneloginScript}
      '';
      executable = true;
      target = ".local/bin/aws-login";
    };
  };

  home.sessionVariables = secrets.work.envs;

  home.packages = with pkgs; [
    ansible
    teleport
    onelogin-aws-assume-role
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
