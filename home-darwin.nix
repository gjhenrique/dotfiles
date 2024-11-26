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

  home.sessionVariables = {
    PINENTRY_PROGRAM = "${stable.pinentry-gtk2}/bin/pinentry-gtk-2";
    EDITOR = "nvim";
  };

  # For linux, NixOS is responsible for managing the fonts
  fonts.fontconfig.enable = true;
}
