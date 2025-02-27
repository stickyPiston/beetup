{ pkgs ? import <nixpkgs> {} }:
let 
  elmDeps = with pkgs.elmPackages; [
    elm
    elm-test
    elm-review
    elm-format
  ];
  shellDeps = with pkgs; [
    git
    live-server
  ];
in pkgs.mkShell {
    buildInputs = shellDeps ++ elmDeps;
  }