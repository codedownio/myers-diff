{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ffcb1ea6c63555a76323586de02a2887f7d7f36b";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        default = pkgs.haskell.packages.ghc943.callPackage ./default.nix {};
      in
        rec {
          packages = rec {
            inherit default;
            inherit (pkgs) cabal2nix;
          };

          defaultPackage = packages.default;

          nixpkgsPath = pkgs.path;
        }
    );
}
