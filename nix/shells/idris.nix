{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ idris2 ]; }
