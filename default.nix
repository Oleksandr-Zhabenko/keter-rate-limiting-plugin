{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "keter-rate-limiting-plugin" ./. {}
