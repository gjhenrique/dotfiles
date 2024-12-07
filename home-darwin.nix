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
    emacs29-macport
    jetbrains-mono
    libgccjit
    gcc
  ];

  home.file = {
    ".ssh/config.overrides".text = builtins.replaceStrings ["@1p-agent@"] [secrets.work.mac1pAgent] sshOverrides;

    "emacs-gui" = {
      text = "${pkgs.emacs29-macport}/Applications/Emacs.app/Contents/MacOS/Emacs \"$@\"";
      target = ".local/bin/emacs-gui";
      executable = true;
    };
  };

  # For linux, NixOS is responsible for managing the fonts
  fonts.fontconfig.enable = true;
}
