{ pkgs ? import ./nix {} }:
{
  inherit (pkgs.canonixHaskellPackages) canonix;
}
