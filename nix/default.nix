{ sources ? import ./sources.nix }:

let
  overlay = self: super: {
    inherit (import sources.niv { inherit pkgs; }) niv;
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
    
    canonixHaskellPackages = super.haskellPackages.extend (hself: hsuper: {
      canonix =
        hsuper.callCabal2nix "canonix" (self.gitignoreSource ../canonix) {};
      haskell-tree-sitter =
        hsuper.callCabal2nix "haskell-tree-sitter" (super.callPackage ./src/haskell-tree-sitter.nix {}) {};
      tree-sitter-nix =
        hsuper.callCabal2nix "tree-sitter-nix" ../tree-sitter-nix {};
    });
  };
  config = {};
  overlays = [overlay];
  pkgs = import sources.nixpkgs { inherit config overlays; };
in 
  pkgs
