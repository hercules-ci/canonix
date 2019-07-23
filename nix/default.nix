{ sources ? import ./sources.nix }:

let
  overlay = self: super: let
      haskell-tree-sitterSource = super.callPackage ./src/haskell-tree-sitter.nix {};
      # haskell-tree-sitterSource = self.gitignoreSource ../../haskell-tree-sitter;
      hlib = super.haskell.lib;
   in {
    inherit (import sources.niv { inherit pkgs; }) niv;
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
    
    canonixHaskellPackages = super.haskellPackages.extend (hself: hsuper: {
      canonix =
        hsuper.callCabal2nix "canonix" (self.gitignoreSource ../canonix) {};
      # TODO: remove dontCheck: test compile error: No instance for (Control.Monad.Fail.MonadFail (PropertyT IO))
      tree-sitter =
        hsuper.callCabal2nix "tree-sitter" (haskell-tree-sitterSource + "/tree-sitter") {
          hedgehog =  hsuper.callCabal2nix "hedgehog" (sources.haskell-hedgehog + "/hedgehog") {};
        };
      tree-sitter-nix =
        hsuper.callCabal2nix "tree-sitter-nix" (haskell-tree-sitterSource + "/tree-sitter-nix") {};
      fused-effects =
         # hsuper.callHackage "fused-effects" "0.4.0.0" {};
         # not in nixpkgs' hackage metadata yet
         hsuper.callCabal2nix "fused-effects" sources.fused-effects {};
    });
  };
  config = {};
  overlays = [overlay];
  pkgs = import sources.nixpkgs { inherit config overlays; };
in 
  pkgs
