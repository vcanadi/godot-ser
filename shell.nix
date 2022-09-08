{ withHLS ? false
, pkgs ? import ./nixpkgs.nix
}:
let
  u = import ./util.nix {};
  main = import ./default.nix {};
  ghc = pkgs.haskellPackages.ghcWithPackages
          (ps:  main.getCabalDeps.executableHaskellDepends
             ++ u.whenFlag withHLS ps.haskell-language-server
          );
in
pkgs.stdenv.mkDerivation {
  name = "local-shell-" + (builtins.baseNameOf ./.)  ;
  buildInputs = [ ghc pkgs.ghcid];
}
