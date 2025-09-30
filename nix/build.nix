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
  musl,
  gcc,
  pkg-config,
  static ? false,
  ...
}:
let
  static_libxml2 = (libxml2.overrideAttrs (old: {
    configureFlags = (old.configureFlags or []) ++ [
      "--enable-static"
      "--with-pic"
      "--without-lzma"
    ];
  }));
in
rustPlatform.buildRustPackage {
  pname = "compiler";
  version = "0.0.1";
  src = ./..;

  cargoLock = {
    lockFile = ../Cargo.lock;
  };

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    llvm
    stdenv.cc.cc.lib
  ] ++ (if static then [
    musl
    musl.dev
  ] else [
    libxml2.dev
    zlib
  ]);

  preBuild = ''
    export LLVM_SYS_150_PREFIX="${llvm_dev}"
    export NIX_LDFLAGS="-L${libffi.out}/lib -lffi"
    ${if static then ''
      export PKG_CONFIG_ALL_STATIC=1
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${static_libxml2.dev}/lib/pkgconfig"

      export PATH=${llvmPackages.clang}/bin:$PATH
      export RUSTFLAGS="-C linker=${musl.dev}/bin/musl-clang -C target-feature=+crt-static -L ${llvmPackages.libcxx.out}/lib -L ${stdenv.cc.cc.lib}/lib -L ${zlib.static}/lib $(pkg-config --libs-only-L libxml-2.0)"
    '' else ""}
  '';

  cargoBuildFlags = lib.optional static "--target=x86_64-unknown-linux-musl";
  doCheck = false;
}
