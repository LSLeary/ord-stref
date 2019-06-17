let
  hostPkgs = import <nixpkgs> {};

  pinnedVersion = hostPkgs.lib.importJSON ./revision.json;

  src = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    inherit (pinnedVersion) rev sha256;
  };
in
  import src
