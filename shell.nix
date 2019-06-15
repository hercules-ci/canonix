{ pkgs ? import ./nix {} }:
pkgs.canonixHaskellPackages.shellFor {
  name = "dev-shell";

  # Make dependencies available
  packages = p: [p.canonix];

  buildInputs = [
    pkgs.niv
    pkgs.cabal-install
  ];
}
