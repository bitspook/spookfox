{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [];
        pkgs = import nixpkgs { inherit system overlays; };

        buildInputs = with pkgs; [ ];

        nativeBuildInputs = with pkgs; [
          nodejs
          yarn
          websocat
        ];

      in
      {
        devShell = pkgs.mkShell
          ({
            inherit buildInputs nativeBuildInputs;
          });
      });
}
