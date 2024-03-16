let
  pkgs = import <nixpkgs> {};
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
{ ghc }:
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "haskell-stack-nix";
  buildInputs = [
      pkgs.mesa
      pkgs.libGL
      pkgs.zlib
      pkgs.pkg-config
      pkgs.glew
      pkgs.freetype
      pkgs.freeglut
      pkgs.SDL2
  ];
}
