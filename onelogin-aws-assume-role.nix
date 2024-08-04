{
  config,
  lib,
  dream2nix,
  ...
}: {
  imports = [
    dream2nix.modules.dream2nix.pip
  ];

  deps = {nixpkgs, ...}: {
    python = nixpkgs.python311;
  };

  name = "onelogin-aws-assume-role";
  version = "1.10.1";

  buildPythonPackage = {
    format = "wheel";
  };

  pip = {
    requirementsList = [
      "${config.name}==${config.version}"
    ];
  };
}
