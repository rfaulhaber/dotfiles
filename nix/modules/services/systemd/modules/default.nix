{
  lib,
  pkgs,
}: let
  fileNames = builtins.filter (f: f != "default.nix") (builtins.attrNames (builtins.readDir ./.));
  imports =
    builtins.map (mod: import (./. + "/${mod}") {inherit lib pkgs;}) fileNames;
  modules = builtins.map (mod: {"${mod.name}" = mod;}) imports;
in
  builtins.foldl' (left: right: left // right) {} modules
