{
  description = "Typescripten";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
        pkgs_static =
          (import inputs.nixpkgs {
            inherit system;
            static = true;
          }).pkgsMusl;

        treefmtconfig = inputs.treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            toml-sort.enable = true;
            yamlfmt.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            prettier.enable = true;
          };
          settings.formatter.shellcheck.excludes = [ ".envrc" ];
        };
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
              ]
            );
            RUSTFLAGS = "-L ${pkgs.libffi}/lib -l ffi";
            LLVM_SYS_150_PREFIX = "${pkgs.llvm.dev}";
          };
          default = llvm-rs;
        };
        formatter = treefmtconfig.config.build.wrapper;
        checks = {
          formatter = treefmtconfig.config.build.check self;
        };
        packages = rec {
          build = (pkgs.callPackage ./nix/build.nix { }) // {
            static = pkgs_static.callPackage ./nix/build.nix { static = true; };
          };
          docker = (pkgs.callPackage ./nix/docker.nix { app = build; }) // {
            static = pkgs.callPackage ./nix/docker.nix {
              app = build.static;
              static = true;
            };
          };
          default = build;
        };
      }
    );
}
