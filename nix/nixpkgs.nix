{ system ? builtins.currentSystem,
  nixBase ? "18.09"
}:
let
  nixPkgsRevSha = import (./. + "/${nixBase}.nix");
  nixpkgs = import ./fetchNixPkgs.nix nixPkgsRevSha;
in
  import nixpkgs { inherit system; }
