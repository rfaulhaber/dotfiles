{ pkgs ? import <nixpkgs>, ... }:

with pkgs;

mkShell {
  buildInputs = [
    nodejs_latest
    nodePackages_latest.prettier
    nodePackages_latest.javascript-typescript-langserver
    nodePackages_latest.yarn
  ];
}
