{ pkgs     ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

let hpkg = pkgs.haskell.packages."${compiler}"; in

hpkg.developPackage {
  root = ./ord-stref;
  returnShellEnv = false;
}
