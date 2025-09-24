{
  rustPlatform,
  libxml2,
  libffi,
  zlib,
  libllvm,
  stdenv,
  ...
}:
rustPlatform.buildRustPackage {
  pname = "compiler";
  version = "0.0.1";
  src = ./..;

  cargoLock = {
    lockFile = ../Cargo.lock;
  };

  buildInputs = [
    libxml2
    zlib
    libllvm
    stdenv.cc.cc.lib
  ];

  LLVM_SYS_150_PREFIX = "${libllvm.dev}";
  NIX_LDFLAGS = "-L${libffi.out}/lib -lffi";
}
