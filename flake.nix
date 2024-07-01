{
  description = "hm-typecheck";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = ins:
    ins.flake-utils.lib.eachDefaultSystem (system : let
      pkgs = import ins.nixpkgs { inherit system; };
      gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
      ghcPkgs = pkgs.haskell.packages.ghc925;
      hmPkgs = ghcPkgs.override {
        overrides = self: super: rec {
          hm = (self.callCabal2nix "hm" (gitignore ./.) {});
          megaparsec = super.megaparsec_9_6_1;
        };
      };
    in {
      packages = rec {
        hm = hmPkgs.hm;
        default = hm;
      };
      devShells.default = ghcPkgs.shellFor {
        packages = _: [ hmPkgs.hm ];
        buildInputs = with ghcPkgs; [
          cabal-install
          haskell-language-server
        ];
      };
    });
}

