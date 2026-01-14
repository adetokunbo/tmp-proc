{
  # Pin haskell-nix to a recent (as of 2025/06/21) commit
  h8x-commit = "a3f435537829d97a1d2b1d27675963ac3ab21151";

  # Pin hackage-nix to the potentially more recent version of hackage
  hackage-nix-commit = "c4a5e33bcb9a1a605d4fb2a2f55ba85dd0965afc";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc98";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2025-06-20T00:00:00Z";
}
