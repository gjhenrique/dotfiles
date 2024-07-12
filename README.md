# dotfiles

Personals setup using NixOS, Hyprland and Emacs

## Setup

It would be awesome to store, but not everything on my machine is declarative.

Folow the additional steps to have a productive machine

1. On this repo, type `git-crypt unlock /tmp/key` (Get the key in bitwarden)
1. `sudo nixos-rebuild switch --flake .` for NixOS configuration
1. `nix run  -- home-manager switch -b backup --flake ".#guilherme"` for home-manager configuration
1. Sign in to firefox
1. Enter tailnet to access internal services
1. Configure syncthing
1. `atuin login` and `atuin sync` for syncing history
1. `alt-A=I` to install tmux plugins with tpm
1. Add passage to have secrets (not including anything because of security through obscurity
1. Generate a gpg key with `gpg --quick-generate-key "Guilherme Henrique <email@email.com>"` and create `~/.authinfo.gpg`

### Git forge
1. Generate a new SSH key with `ssh-keygen -t ed25519 -C "name@host"`
