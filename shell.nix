{ withHLS ? false   # Include haskell-language-server in nix-shell
, forSelf ? withHLS # Include the package itself in the development shell
, forTests ? withHLS  # Include test dependencies in nix-shell (for working on *Spec files)
, forExecs ? withHLS
, forLibs ? withHLS
, pkgs ? import ./nixpkgs.nix
}:
let
  u = import ./util.nix {};
  this = import ./default.nix {};
  ghc = pkgs.haskellPackages.ghcWithPackages
          (ps:  (if forSelf then [this] else [])
             ++ (if forTests then this.getCabalDeps.testHaskellDepends       else [])
             ++ (if forExecs then this.getCabalDeps.executableHaskellDepends else [])
             ++ (if forLibs  then this.getCabalDeps.libraryHaskellDepends    else [])
             ++ u.whenFlag withHLS ps.haskell-language-server
          );
in
pkgs.stdenv.mkDerivation {
  name = "local-shell-" + (builtins.baseNameOf ./.)  ;
  buildInputs = [ ghc pkgs.haskellPackages.ghcid ];
}

