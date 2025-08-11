{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.mkDerivation {
  pname = "keter-rate-limiting-plugin";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    aeson 
    base 
    bytestring 
    cache 
    clock 
    hashable 
    http-types 
    network 
    text 
    cookie
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
    unordered-containers
    http-types 
    network 
    tasty 
    HUnit 
    QuickCheck
    cookie
    tasty-quickcheck
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
  testTargets = [ "--test-options='--num-threads=1'" "--test-options='--color=always'" ]; # Run tests sequentially
  homepage = "https://github.com/Oleksandr-Zhabenko/keter-rate-limiting-plugin";
  description = "Simple Keter rate limiting plugin";
  license = pkgs.lib.licenses.mit;
}

