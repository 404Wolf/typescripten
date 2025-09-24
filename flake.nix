{
  description = "CSDS 377 Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      ...
    }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        fenix_pkg = inputs.fenix.packages.${system};
        rustComponents = [
          "cargo"
          "rust-src"
          "rustc"
          "rustfmt"
        ];
        toolchain = (fenix_pkg.complete.withComponents rustComponents);
      in
      {
        devShells = rec {
          llvm-rs = pkgs.mkShell {
            shellHook = ''
              export LD_LIBRARY_PATH="${pkgs.libffi}/lib:${pkgs.stdenv.cc.cc.lib}/lib"
            '';
            packages = (
              with pkgs;
              [
                libxml2.dev
                libffi.dev
                libffi
                nil
                nixd
                llvm
                toolchain
                fenix_pkg.rust-analyzer
                clang
              ]
            );
            RUSTFLAGS = "-L ${pkgs.libffi}/lib -l ffi";
          };
          default = llvm-rs;
        };
        formatter =
          let
            treefmtconfig = inputs.treefmt-nix.lib.evalModule pkgs {
              projectRootFile = "flake.nix";
              programs = {
                nixfmt.enable = true;
                toml-sort.enable = true;
                yamlfmt.enable = true;
                mdformat.enable = true;
                shellcheck.enable = true;
                shfmt.enable = true;
              };
              settings.formatter.shellcheck.excludes = [ ".envrc" ];
            };
          in
          treefmtconfig.config.build.wrapper;
        packages = rec {
          build = pkgs.callPackage ./nix/build.nix { inherit toolchain; };
          default = build;
        };
      }
    );
}
