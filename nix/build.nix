{ stdenv, toolchain, makeRustPlatform, ... }:
(makeRustPlatform {
  cargo = toolchain;
  rustc = toolchain;
}).buildRustPackage {
  pname = "compiler";
  version = "0.0.1";
  src = ./..;
  cargoLock.lockFile = ../Cargo.lock;
}
