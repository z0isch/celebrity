{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    frontend = ./frontend;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };

})
