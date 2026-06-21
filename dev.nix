{
  # Pin haskell-nix to a recent (as of 2025/06/21) commit
  h8x-commit = "a3f435537829d97a1d2b1d27675963ac3ab21151";

  # Pin hackage-nix to the potentially more recent version of hackage (as if 2026/06/10)
  hackage-nix-commit = "cc1f434e3407d58970261cbff2e4a5dab00b803b";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc98";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2026-06-10T00:00:00Z";
}
