{
  pkgs,
  stable,
  system,
  ...
}: let
in {
  home.packages = with pkgs; [
    emacs29-macport
    jetbrains-mono
  ];


  home.file = {
    "emacs" = {
      text = "${pkgs.emacs29-macport}/Applications/Emacs.app/Contents/MacOS/Emacs \"$@\"";
      target = ".local/bin/emacs";
      executable = true;
    };
  };

  # For linux, NixOS is responsible for managing the fonts
  fonts.fontconfig.enable = true;
}
