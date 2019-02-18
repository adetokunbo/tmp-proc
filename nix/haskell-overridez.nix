let
  pkgs = import <nixpkgs> {};
  overridez = fetchTarball {
    url = "https://github.com/adetokunbo/haskell-overridez/archive/v0.10.3.1.tar.gz";
    sha256 = "1w2pv96bdf16nc1vvh03acx37qq4h4hrz2y979a705i70z8h59lk";
  };
in
  import overridez { inherit pkgs; }
