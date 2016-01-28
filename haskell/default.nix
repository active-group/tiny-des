{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = nixpkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          MonadRandom heap
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "des";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
