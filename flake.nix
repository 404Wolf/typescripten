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
        pkgs_static = import inputs.nixpkgs { inherit system; static = true; };
        toolchain = with inputs.fenix.packages.${system}; combine [
            latest.cargo
            latest.rustc
            targets."x86_64-unknown-linux-musl".latest.rust-std
          ];

        # toolchain = inputs.fenix.packages.${system}.stable.withTargets [ "x86_64-unknown-linux-musl" ];
        staticRustPlatform = pkgs.makeRustPlatform {
          cargo = toolchain;
          rustc = toolchain;
        };
        static_llvm = pkgs_static.llvmPackages_15.llvm.override {
          stdenv = pkgs_static.pkgsMusl.stdenv;
        };
        llvmWrapped = pkgs.writeShellScriptBin "llvm-config" ''
          exec ${static_llvm.dev}/bin/llvm-config "$@" | sed 's/-lrt//g; s/-ldl//g; s/-lm//g; s/-lc++//g; s/-lstdc++//g; s/-lc++abi//g'
        '';
      in
      {
        devShells = rec {
          llvm-rs = pkgs.mkShell {
            shellHook = ''
              export LD_LIBRARY_PATH="${pkgs.libffi}/lib:${pkgs.stdenv.cc.cc.lib}/lib"
            '';
            packages = (
              with pkgs_static;
              [
                libxml2.dev
                libffi.dev
                libffi
                nil
                nixd
                static_llvm
                static_llvm.dev
                clang
                toolchain
              ]
            );
            RUSTFLAGS = "-L ${pkgs.libffi}/lib -l ffi";
            LLVM_SYS_150_PREFIX="${static_llvm.dev}";
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
          build = pkgs.callPackage ./nix/build.nix { llvm_dev = pkgs.llvm.dev; };
          build_static = pkgs_static.pkgsMusl.callPackage ./nix/build.nix {
            static = true;
            rustPlatform = staticRustPlatform;
            stdenv = pkgs_static.pkgsMusl.stdenv;
            llvm = static_llvm;
            llvm_dev = llvmWrapped;
          };
          docker = pkgs.callPackage ./nix/docker.nix { app = build; };
          default = build;
        };
      }
    );
}
