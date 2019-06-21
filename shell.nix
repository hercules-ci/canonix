{ pkgs ? import ./nix {} }:
let
  hs = pkgs.canonixHaskellPackages;
in
pkgs.canonixHaskellPackages.shellFor {
  name = "dev-shell";

  # Make dependencies available
  packages = p: [p.canonix];

  buildInputs = [
    pkgs.niv
    hs.cabal-install
    hs.ghcid
  ];

  # stack hardcodes <nixpkgs>; possibly other tools
  NIX_PATH = "nixpkgs=${pkgs.path}";
}
