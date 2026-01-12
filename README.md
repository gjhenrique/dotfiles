# dotfiles

Personals setup using NixOS, Hyprland and Emacs

## steps

1. On this repo, type `git-crypt unlock /tmp/key` (Get the key in bitwarden)
1. `sudo nixos-rebuild switch --flake .` for NixOS configuration
1. `nix run  -- home-manager switch -b backup --flake ".#guilherme@[darwin|linux]"` for home-manager configuration

## Impurities

It would be awesome to store everyting declaratively, but doing for some tasks would be either too time consuming or impossible.

Here are some of those:

### Services
1. Sign in to firefox
1. Sign in to tailscale with `tailscale login` to access internal services
1. Configure syncthing
1. `atuin login` and `atuin sync` for syncing history
1. `alt-A=I` to install tmux plugins with tpm

### Secrets
1. `mkdir -p $HOME/.passage`
1. Run `age-plugin-yubikey --identity >> $HOME/.passage/identities`
1. Clone the repo in (you know where it is ;)) `~/.passage/store`

## Update only nixpkgs-edge

To avoid disrupting my work during the week, I would like to only add some packages to the latest version.

Instead of running `nix flake update` to update all inputs, use this to only update the latest version.

``` shell
nix flake lock --update-input nixpkgs-edge
```
