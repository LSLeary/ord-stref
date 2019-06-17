{ pkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:

let
  # hlib = pkgs.haskell.lib;
  hpkg = pkgs.haskell.packages."${compiler}";
in

hpkg.developPackage {
  root = ./ord-stref;
  returnShellEnv = false;
}
