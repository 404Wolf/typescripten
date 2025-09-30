{
  lib,
  rustPlatform,
  libxml2,
  libffi,
  zlib,
  llvm,
  llvmPackages,
  stdenv,
  musl,
  gcc,
  pkg-config,
  writeShellScriptBin,
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
  static_libffi = (libffi.overrideAttrs (old: {
    configureFlags = (old.configureFlags or []) ++ [
      "--enable-static"
      "--disable-shared"
      "--with-pic"
    ];
  }));
  llvmWrapped = writeShellScriptBin "llvm-config" ''
    exec ${llvm.dev}/bin/llvm-config "$@" | sed 's/-lrt//g; s/-ldl//g; s/-lm//g'
  '';
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
    ${if static then ''
      export LLVM_SYS_150_PREFIX="${llvmWrapped}"
      export NIX_LDFLAGS="-L${static_libffi.out}/lib -lffi"
      export PKG_CONFIG_ALL_STATIC=1
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${static_libxml2.dev}/lib/pkgconfig"

      export RUSTFLAGS="-C target-feature=+crt-static -C relocation-model=static -L ${llvmPackages.libcxx.out}/lib -L ${stdenv.cc.cc.lib}/lib -L ${zlib.static}/lib $(pkg-config --libs-only-L libxml-2.0)"
    '' else ''
      export LLVM_SYS_150_PREFIX="${llvm.dev}"
      export NIX_LDFLAGS="-L${libffi.out}/lib -lffi"
    ''}
  '';


  cargoBuildFlags = lib.optional static "--target=x86_64-unknown-linux-musl";
  doCheck = false;
}
