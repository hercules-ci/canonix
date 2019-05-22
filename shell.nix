{ pkgs ? import ./nix {} }:
pkgs.mkShell {
  name = "dev-shell";
  buildInputs = [
    pkgs.niv
    pkgs.cabal-install
    (pkgs.canonixHaskellPackages.ghcWithPackages (p: [p.haskell-tree-sitter p.tree-sitter-nix]))
  ];
}
