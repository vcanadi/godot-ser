{ withHLS ? false
, pkgs ? import ./nixpkgs.nix
}:
let
  u = import ./util.nix {};
  this = import ./default.nix {};
  ghc = pkgs.haskellPackages.ghcWithPackages
          (ps:  this.getCabalDeps.libraryHaskellDepends
             ++ this.getCabalDeps.executableHaskellDepends
             ++ this.getCabalDeps.testHaskellDepends
             ++ u.whenFlag withHLS ps.haskell-language-server
             ++ [this]
          );
in
pkgs.stdenv.mkDerivation {
  name = "local-shell-" + (builtins.baseNameOf ./.)  ;
  buildInputs = [ ghc pkgs.ghcid this pkgs.cabal-install];
}
