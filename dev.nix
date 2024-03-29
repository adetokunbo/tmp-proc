{
  # Pin haskell-nix to a recent (as of 2023/11/24) commit
  h8x-commit = "52112cd0c964ccfc2d6ca26d4c44ec42055e986f";

  # Pin hackage-nix to the potentially more recent version of hackage
  hackage-nix-commit = "e08be045443108c50a10fbc4abcf5d1d20a13625";

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state which should be supported by either
  # the h8x-commit, or the hackage-nix-commit, whichever is latest
  index-state = "2024-03-24T00:00:00Z";
}
