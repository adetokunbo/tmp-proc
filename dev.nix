{
  # Pin haskell-nix to a recent (as of 2024/05/04) commit
  h8x-commit = "5297ad9e688b8a6d1ae0e8297f5502bccb5511a5";

  # Pin hackage-nix to the potentially more recent version of hackage
  hackage-nix-commit = "25618a5293d11ef32e37bc10070783c3f720fa3a";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2024-05-12T00:00:00Z";
}
