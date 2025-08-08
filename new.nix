{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.mkDerivation {
  pname = "keter-rate-limiting-plugin";
  version = "0.1.0.2";
  src = ./.;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    aeson 
    base 
    bytestring 
    cache 
    clock 
    containers
    hashable 
    http-types 
    network 
    text 
    case-insensitive 
    network 
    focus
    time 
    wai
    iproute 
    list-t 
    stm-containers 
    stm
  ];
  librarySystemDepends = [ pkgs.zlib ];
  testHaskellDepends = with pkgs.haskellPackages; [
    base 
    bytestring 
    containers 
    http-types 
    network 
    tasty 
    HUnit 
    case-insensitive
    tasty-hunit 
    wai-extra 
    temporary 
    text 
    wai 
    network 
    stm-containers 
    stm 
    async 
    random
  ];
  testTargets = [ "--test-options='--num-threads=1'" "--test-options='--color=always'" ]; #"--test-options='--hide-successes'" ]; # Run tests sequentially
  homepage = "https://github.com/Oleksandr-Zhabenko/keter-rate-limiting-plugin";
  description = "Simple Keter rate limiting plugin";
  license = pkgs.lib.licenses.mit;
}

