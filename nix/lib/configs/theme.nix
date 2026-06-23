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
}: {
  themeName,
  # Host-level palette corrections, keyed by color name (e.g. `yellow`), values
  # `#`-prefixed. Applied last so an explicit host override beats both the
  # base16 slot and any per-theme custom file — used to fix base16 schemes whose
  # named ramp is mislabelled (tokyo-night-dark's `yellow` slot is a blue).
  overrides ? {},
}: let
  base16 = pkgs.callPackage inputs.base16.lib {};
  schemePath = "${inputs.tt-schemes}/base16/${themeName}.yaml";
  base = base16.mkSchemeAttrs schemePath;

  # Semantic aliases not provided by the base16 scheme itself, derived from
  # the standard base16 slot meanings (base00 = bg ... base05 = fg, base08-0F
  # = the accent ramp). Consumers (niri, fuzzel, nushell, themeAttrs) reference
  # these names, so deriving them here lets any base16 theme work without a
  # hand-written per-theme override file. `scheme` is either `base` (raw hex)
  # or `base.withHashtag` (hex with leading '#'); the alias values inherit
  # whichever form was passed.
  semanticAliases = scheme: {
    bg = scheme.base00;
    bg-alt = scheme.base01;
    fg = scheme.base05;
    fg-alt = scheme.base06;
    grey = scheme.base03;
    teal = scheme.base0C;
    dark-cyan = scheme.base0C;
    dark-blue = scheme.base0D;
    violet = scheme.base0E;
    bright-black = scheme.base03;
    bright-white = scheme.base07;
  };

  customPath = themesDir + "/${themeName}.nix";
  hasCustom = builtins.pathExists customPath;
  custom =
    if hasCustom
    then import customPath
    else {};
  customNoHash = builtins.mapAttrs (_: v: lib.removePrefix "#" v) custom;
  overridesNoHash = builtins.mapAttrs (_: v: lib.removePrefix "#" v) overrides;
in
  base
  // semanticAliases base
  // customNoHash
  // overridesNoHash
  // {
    withHashtag = base.withHashtag // semanticAliases base.withHashtag // custom // overrides;
  }
