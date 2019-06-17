{ pkgs     ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

(import ./. { inherit pkgs compiler; }).env
