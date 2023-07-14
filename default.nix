let
  # Pin haskell-nix to a recent (as of 2023/06/16) commit
  h8x-pin = "https://github.com/input-output-hk/haskell.nix/archive/0b9d00931f7a80c62378b50c554cb681af690365.tar.gz";
  h8x-src = builtins.fetchTarball h8x-pin;
  h8x = import h8x-src {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these. But you
    # can also just use your own, e.g. '<nixpkgs>'.

    h8x.sources.nixpkgs-2305

    # These arguments passed to nixpkgs, include some patches and also the
    # haskell.nix functionality itself as an overlay.

    h8x.nixpkgsArgs;

in pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "tmp-proc";
    src = ./.;
  };

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state
  index-state = "2023-07-07T00:00:00Z";
}
