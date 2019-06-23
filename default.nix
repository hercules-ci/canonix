{ pkgs ? import ./nix {} }:
{
  canonix =
    pkgs.haskell.lib.justStaticExecutables pkgs.canonixHaskellPackages.canonix;
  
  canonix-lib = pkgs.canonixHaskellPackages.canonix;
  canonix-strict =
    pkgs.haskell.lib.buildStrictly pkgs.canonixHaskellPackages.canonix;
  
}
