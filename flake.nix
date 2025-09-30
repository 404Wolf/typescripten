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
        pkgs_static = (import inputs.nixpkgs {
          inherit system;
          static = true;
        }).pkgsMusl;
        toolchain = with inputs.fenix.packages.${system}; combine [
            latest.cargo
            latest.rustc
            targets."x86_64-unknown-linux-musl".latest.rust-std
          ];

        # toolchain = inputs.fenix.packages.${system}.stable.withTargets [ "x86_64-unknown-linux-musl" ];
        # staticRustPlatform = pkgs_static.makeRustPlatform {
        #   cargo = pkgs_static.cargo;
        #   rustc = pkgs_static.rustc;
        #   # cargo = toolchain;
        #   # rustc = toolchain;
        # };
        llvmWrapped = pkgs_static.writeShellScriptBin "llvm-config" ''
          exec ${pkgs_static.llvm.dev}/bin/llvm-config "$@" | sed 's/-lrt//g; s/-ldl//g; s/-lm//g'
        '';
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
                llvm.dev
                clang
                toolchain
              ]
            );
            RUSTFLAGS = "-L ${pkgs.libffi}/lib -l ffi";
            LLVM_SYS_150_PREFIX="${pkgs.llvm.dev}";
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
          build_static = pkgs_static.callPackage ./nix/build.nix {
            static = true;
            # rustPlatform = staticRustPlatform;
            llvm_dev = llvmWrapped;
          };
          docker = pkgs.callPackage ./nix/docker.nix { app = build; };
          default = build;
        };
      }
    );
}
