{pkgs}:
pkgs.stdenv.mkDerivation rec {
  name = "tmux-tpm";
  rev = "99469c4a9b1ccf77fade25842dc7bafbc8ce9946";
  version = "v1";

  src = pkgs.fetchFromGitHub {
    inherit rev;
    owner = "tmux-plugins";
    repo = "tpm";
    sha256 = "1ap5x761abcpw6wd6jb575rws88prkpjygjks9cibvws59xsnki4";
  };

  builder = ./builder.sh;
}
