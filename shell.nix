{ pkgs ? import ./nix {} }:
pkgs.mkShell {
  name = "dev-shell";
  buildInputs = [ pkgs.niv pkgs.cabal-install pkgs.ghc ];
}
