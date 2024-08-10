# dotfiles

Personals setup using NixOS, Hyprland and Emacs

## steps

1. On this repo, type `git-crypt unlock /tmp/key` (Get the key in bitwarden)
1. `sudo nixos-rebuild switch --flake .` for NixOS configuration
1. `nix run  -- home-manager switch -b backup --flake ".#guilherme"` for home-manager configuration

## Impurities

It would be awesome to store everyting declaratively, but doing for some tasks would be either too time consuming or impossible.

Here are some of those:

1. Sign in to firefox
1. Sign in to tailscale with `tailscale loginh` to access internal services
1. Configure syncthing
1. `atuin login` and `atuin sync` for syncing history
1. `alt-A=I` to install tmux plugins with tpm
1. Add passage to have secrets. You know the repo where the secrets are stored ;)
1. Generate a gpg key with `gpg --quick-generate-key "Emacs <emacs@gjhenrique.com>"` and create `~/.authinfo.gpg` with the right credentials
