#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-git

nix-prefetch-git https://github.com/nixos/nixpkgs-channels \
  --rev refs/heads/nixos-unstable > "revision.json"
