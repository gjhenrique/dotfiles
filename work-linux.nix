{pkgs, edgePkgs }: {
  home.packages = with pkgs; [
    _1password-gui
    slack
    spotify
  ] ++ [
      edgePkgs.vscode
   ];
}
