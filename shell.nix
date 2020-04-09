let
  pkgs = import <nixpkgs> {};
  drv = import ./release.nix;
in drv.env.overrideAttrs (attrs: {
  buildInputs = [ pkgs.cabal-install pkgs.ghcid ] ++ attrs.buildInputs;
})
