{ pkgs ? import ./nixpkgs.nix, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
hpkgs.callCabal2nix (builtins.baseNameOf ./.) ./. { inherit mkDerivation; }
