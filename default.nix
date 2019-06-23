{ pkgs ? import ./nix {} }:
{
  canonix =
    pkgs.haskell.lib.justStaticExecutables pkgs.canonixHaskellPackages.canonix;
  
}
