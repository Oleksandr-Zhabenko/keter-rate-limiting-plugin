{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.mkDerivation {
  pname = "keter-rate-limiting-plugin";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    aeson base base64-bytestring bytestring cache clock containers
    hashable http-types network text time wai mtl list-t stm-containers stm
  ];
  testHaskellDepends = with pkgs.haskellPackages; [
    base bytestring containers http-types network tasty HUnit tasty-hunit
    text wai stm-containers stm async random
  ];
  homepage = "https://github.com/Oleksandr-Zhabenko/keter-rate-limiting-plugin";
  description = "Simple Keter rate limiting plugin";
  license = pkgs.lib.licenses.mit;
}

