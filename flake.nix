{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # GHC version used by the resolver in stack.yaml (nightly-2025-05-13).
        haskellPackages = pkgs.haskell.packages.ghc9102;

        default = haskellPackages.callPackage ./default.nix {};
      in
        rec {
          packages = rec {
            inherit default;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.default;

          devShells.default = pkgs.mkShell {
            name = "myers-diff";

            nativeBuildInputs = [
              haskellPackages.ghc
              pkgs.cabal-install
              pkgs.stack
              pkgs.hpack
              pkgs.hlint
              pkgs.pkg-config
            ];

            buildInputs = [
              pkgs.zlib
            ];
          };

          nixpkgsPath = pkgs.path;
        }
    );
}
