# dotfiles

My dotfiles

## Setup
1. `sudo nixos-rebuild switch --flake .` for NixOS configuration
1. `git-crypt unlock /tmp/key` (Get the key from bitwarden)
1. `nix run  -- home-manager switch -b backup --flake \".#guilherme\" --impure"` for home-manager configuration
1. Sign in to firefox
1. Sync syncthing
1. sync atuin with `atuin sync` after `atuin login`

## New steps
1. `alt-A=I` to install tmux plugins with tpm
