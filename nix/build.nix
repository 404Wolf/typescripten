{
  lib,
  rustPlatform,
  libxml2,
  libffi,
  zlib,
  llvm,
  llvmPackages,
  llvm_dev,
  stdenv,
  pkgsMusl,
  musl,
  static ? false,
  ...
}:
rustPlatform.buildRustPackage {
  pname = "compiler";
  version = "0.0.1";
  src = ./..;

  cargoLock = {
    lockFile = ../Cargo.lock;
  };

  # nativeBuildInputs = [
  #   pkg-config
  #   toolchain
  # ];

  buildInputs = [
    libxml2
    zlib
    llvm
    llvmPackages.libcxx
    # stdenv.cc.cc.lib
  ] ++ (if static then [
    musl
    musl.dev
  ] else []);

  preBuild = ''
    export LLVM_SYS_150_PREFIX="${llvm_dev}"
    export NIX_LDFLAGS="-L${libffi.out}/lib -lffi"
    ${if static then ''
      export CARGO_BUILD_TARGET="x86_64-unknown-linux-musl"
      export RUSTFLAGS="-C linker=musl-gcc -C target-feature=+crt-static"
    '' else ""}
  '';

  cargoBuildFlags = lib.optional static "--target=x86_64-unknown-linux-musl";
}
