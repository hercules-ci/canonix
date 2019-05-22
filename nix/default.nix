{ sources ? import ./sources.nix }:

let
  overlay = self: super: let
      haskell-tree-sitterSource = super.callPackage ./src/haskell-tree-sitter.nix {};
      # haskell-tree-sitterSource = self.gitignoreSource ../../haskell-tree-sitter;
   in {
    inherit (import sources.niv { inherit pkgs; }) niv;
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
    
    canonixHaskellPackages = super.haskellPackages.extend (hself: hsuper: {
      canonix =
        hsuper.callCabal2nix "canonix" (self.gitignoreSource ../canonix) {};
      haskell-tree-sitter =
        hsuper.callCabal2nix "haskell-tree-sitter" haskell-tree-sitterSource {};
      tree-sitter-nix =
        hsuper.callCabal2nix "tree-sitter-nix" (haskell-tree-sitterSource + "/languages/nix") {};
    });
  };
  config = {};
  overlays = [overlay];
  pkgs = import sources.nixpkgs { inherit config overlays; };
in 
  pkgs
