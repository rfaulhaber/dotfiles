{
  lib,
  pkgs,
}:
with builtins; let
  fileNames = filter (f: f != "default.nix") (attrNames (readDir ./.));
  imports =
    map (mod: import (./. + "/${mod}") {inherit lib pkgs;}) fileNames;
  modules = map (mod: {"${mod.name}" = mod;}) imports;
in
  foldl' (left: right: left // right) {} modules
