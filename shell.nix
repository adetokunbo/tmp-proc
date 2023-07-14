let
  project = import ./default.nix;
in
  project.shellFor {

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "latest";
      haskell-language-server = {
        version = "latest";
        src = project.pkgs.haskell-nix.sources."hls-2.0";
      };
      ghcid = "latest";
      cabal-fmt = "latest";
    };
  }
