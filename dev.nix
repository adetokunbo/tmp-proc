{
  # Pin haskell-nix to a recent (as of 2023/03/31) commit
  h8x-commit = "52112cd0c964ccfc2d6ca26d4c44ec42055e986f";

  # Pin hackage-nix to the potentially more recent version of hackage
  hackage-nix-commit = "39fbc1a06c422905a573beb2e81db7b73e31e099";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2024-03-31T00:00:00Z";
}
