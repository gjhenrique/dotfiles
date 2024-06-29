# dotfiles

My dotfiles

## Setup

Additional steps to have a productive machine

1. `sudo nixos-rebuild switch --flake .` for NixOS configuration
1. `git-crypt unlock /tmp/key` (Get the key in bitwarden)
1. `nix run  -- home-manager switch -b backup --flake ".#guilherme"` for home-manager configuration
1. Sign in to firefox
1. Enter tailnet
1. Sync syncthing
1. sync atuin with `atuin sync` after `atuin login`
1. `alt-A=I` to install tmux plugins with tpm
