{ pkgs ? import ./nixpkgs.nix {}}:
{
  whenFlag = flag: pkg: if flag then [pkg] else [];
}
