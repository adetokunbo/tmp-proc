let
  dev = import ./dev.nix;
  pkgs-nix = import ./h8x.nix;
in
{ pkgs ? pkgs-nix
} : pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "tmp-proc";
    src = ./.;
  };
  inherit (dev) compiler-nix-name index-state;
}
