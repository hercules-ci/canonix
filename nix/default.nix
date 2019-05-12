{ sources ? import ./sources.nix }:

let
  overlay = self: super: {
    inherit (import sources.niv { inherit pkgs; }) niv;
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
    
    canonixHaskellPackages = super.haskellPackages.extend (hself: hsuper: {
      canonix = hsuper.callCabal2nix "canonix" (self.gitignoreSource ../canonix) {};
    });
  };
  config = {};
  overlays = [overlay];
  pkgs = import sources.nixpkgs { inherit config overlays; };
in 
  pkgs
