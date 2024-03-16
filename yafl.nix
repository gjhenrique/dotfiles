# allow our nixpkgs import to be overridden if desired
{pkgs ? import <nixpkgs> {}}:
# let's write an actual basic derivation
# this uses the standard nixpkgs mkDerivation function
pkgs.stdenv.mkDerivation rec {
  name = "yafl-${version}";

  version = "0.1.0";

  # https://nixos.wiki/wiki/Packaging/Binaries
  src = pkgs.fetchurl {
    url = "https://github.com/gjhenrique/yafl/releases/download/v${version}/yafl_${version}_linux_amd64.tar.gz";
    sha256 = "sha256-VKouPjLoNfLsz/2big5Vi9wwJenbZfD3EhZ6sbiU91k=";
  };

  sourceRoot = ".";

  installPhase = ''
    install -m755 -D yafl $out/bin/yafl
  '';
}
