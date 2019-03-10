{ system ? builtins.currentSystem
, nixBase ? "18.09"
}:
let
  overridez = import ./nix/haskell-overridez.nix;
  pkgsMake = import ./nix/fetchPkgsMake.nix {};
  nixVersion = import (./. + "/nix/${nixBase}.nix");
  nixpkgs = import ./nix/fetchNixPkgs.nix nixVersion;

  pkgsMakeHaskellOverridez = pkgs: overridez.allIn ./nix;
  pkgsMakeArgs = {
    nixpkgsRev = nixVersion.rev;
    nixpkgsSha256 = nixVersion.sha256;
    haskellArgs = {
      overrides = pkgsMakeHaskellOverridez;
      extraOverrides = pkgsMakeHaskellOverridez;
      envMoreTools = [
        pkgs.haskellPackages.apply-refact
        pkgs.haskellPackages.cabal2nix
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
        pkgs.haskellPackages.hlint
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.stylish-cabal
        pkgs.haskellPackages.stylish-haskell
      ];
    };
  };
  pkgs = import nixpkgs { inherit system; };

in

pkgsMake pkgsMakeArgs ({ call, lib, ... }: rec {
  docker-tmp-proc = call.haskell.cabal2nix.lib ./.;
})
