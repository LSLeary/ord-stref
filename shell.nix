{ pkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:

(import ./. { inherit pkgs compiler; }).env
