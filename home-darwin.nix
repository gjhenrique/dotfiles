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

  # For linux, NixOS is responsible for managing the fonts
  fonts.fontconfig.enable = true;
}
