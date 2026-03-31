# Standalone theme resolution: resolves a base16 theme name into
# a full color attrset, merging custom overrides if they exist.
#
# Usage:
#   resolveTheme = import ./theme.nix { inherit pkgs inputs lib; themesDir = ./path/to/themes; };
#   colors = resolveTheme { themeName = "tokyo-night-dark"; };
{
  pkgs,
  inputs,
  lib,
  themesDir,
}: {themeName}: let
  base16 = pkgs.callPackage inputs.base16.lib {};
  schemePath = "${inputs.tt-schemes}/base16/${themeName}.yaml";
  base = base16.mkSchemeAttrs schemePath;
  customPath = themesDir + "/${themeName}.nix";
  hasCustom = builtins.pathExists customPath;
  custom =
    if hasCustom
    then import customPath
    else {};
  customNoHash = builtins.mapAttrs (_: v: lib.removePrefix "#" v) custom;
in
  base
  // customNoHash
  // {
    withHashtag = base.withHashtag // custom;
  }
