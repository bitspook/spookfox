let
  rust_overlay = (import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"));
  nixpkgs = import <nixpkgs> { overlays = [ rust_overlay ]; };
in
with nixpkgs;
mkShell {
  buildInputs = [
    cargo-edit
    rust-analyzer
    nodejs
    yarn
    (rust-bin.stable.latest.default.override {
      extensions = [
        "rust-src"
        "cargo"
        "rustc"
        "rust-analysis"
        "rustfmt"
        "clippy"
      ];
    })
  ];
}
