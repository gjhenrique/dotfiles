{
  pkgs,
  stable,
  system,
  secrets,
  ...
}: let
  sshOverrides = builtins.readFile ./secrets/ssh_overrides;
in {
  home.packages = with pkgs; [
    _1password-cli
    emacs-macport
    gcc
    gnutar
    jetbrains-mono
    libgccjit
  ];


  programs.zsh = {
    shellAliases = {
      tailscale = "/Applications/Tailscale.app/Contents/MacOS/home";
    };
  };

  home.file = {
    ".ssh/config.overrides".text = builtins.replaceStrings ["@1p-agent@"] [secrets.work.mac1pAgent] sshOverrides;

    "emacs-gui" = {
      text = "${pkgs.emacs-macport}/Applications/Emacs.app/Contents/MacOS/Emacs \"$@\"";
      target = ".local/bin/emacs-gui";
      executable = true;
    };
  };

  # For linux, NixOS is responsible for managing the fonts
  fonts.fontconfig.enable = true;
}
