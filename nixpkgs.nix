# Source of nixpkgs i.e. a system "snapshot" for packages and tooling used for building
# To Use local nixpkgs used this instead:
# import <nixpkgs> {}
let
  nixpkgs = import (builtins.fetchTarball
    {  # Descriptive name to make the store path easier to identify
      name = "nixos-unstable-2023-11-26";
      # git ls-remote https://github.com/nixos/nixpkgs <channel>
      url = "https://github.com/nixos/nixpkgs/archive/5631ea37394bb38556cbb2b4142cd28ba49ff2d9.tar.gz";
      # nix-prefetch-url --unpack <url>
      sha256 = "11k114qrmg40mnm81x2hihs7apkffnkrichk3la90718pbb0da03";
    }) ;
  # local = import <nixpkgs>;
in nixpkgs {}
