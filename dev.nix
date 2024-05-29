{
  # Pin haskell-nix to a recent (as of 2024/05/04) commit
  h8x-commit = "5297ad9e688b8a6d1ae0e8297f5502bccb5511a5";

  # Pin hackage-nix to the potentially more recent version of hackage
  hackage-nix-commit = "a62dac66a95f2fb407c824592c6f325b8f7d0b60";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2024-05-13T00:00:00Z";
}
