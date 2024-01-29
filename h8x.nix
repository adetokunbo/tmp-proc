let
  dev = import ./dev.nix;
  inherit (dev) h8x-commit;
  h8x-pin = "https://github.com/input-output-hk/haskell.nix/archive/${h8x-commit}.tar.gz";
  h8x-src = builtins.fetchTarball h8x-pin;
  h8x = import h8x-src {};

in
  import

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these. But you
    # can also just use your own, e.g. '<nixpkgs>'.

    h8x.sources.nixpkgs-unstable

    # These arguments passed to nixpkgs, include some patches and also the
    # haskell.nix functionality itself as an overlay.

    h8x.nixpkgsArgs
