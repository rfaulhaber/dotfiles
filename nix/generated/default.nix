# Builds generated config files for both linux and darwin platforms.
# Output structure:
#   $out/linux/ghostty/{config,theme}
#   $out/linux/nushell/{config.nu,env.nu,generated-theme.nu}
#   $out/darwin/ghostty/{config,theme}
#   $out/darwin/nushell/{config.nu,env.nu,generated-theme.nu}
{
  pkgs,
  lib,
  inputs,
}: let
  defaults = import ./defaults.nix;

  resolveTheme = import ../lib/configs/theme.nix {
    inherit pkgs inputs lib;
    themesDir = ../modules/themes;
  };

  colors = resolveTheme {themeName = defaults.theme;};

  ghosttyConfigs = import ../lib/configs/ghostty.nix {
    colors = colors.withHashtag;
    font = defaults.font;
    fontSize = defaults.ghostty.fontSize;
  };

  nushellConfigs = import ../lib/configs/nushell.nix {
    colors = colors.withHashtag;
    themeName = colors.scheme;
  };

  ghosttyConfig = pkgs.writeText "ghostty-config" ghosttyConfigs.config;
  ghosttyTheme = pkgs.writeText "ghostty-theme" ghosttyConfigs.theme;
  nushellConfig = pkgs.writeText "nushell-config" nushellConfigs.config;
  nushellEnv = pkgs.writeText "nushell-env" nushellConfigs.env;
  nushellTheme = pkgs.writeText "nushell-generated-theme" nushellConfigs.generated-theme;
in
  pkgs.runCommand "generated-configs" {} ''
    for platform in linux darwin; do
      mkdir -p $out/$platform/ghostty $out/$platform/nushell
      cp ${ghosttyConfig} $out/$platform/ghostty/config
      cp ${ghosttyTheme} $out/$platform/ghostty/theme
      cp ${nushellConfig} $out/$platform/nushell/config.nu
      cp ${nushellEnv} $out/$platform/nushell/env.nu
      cp ${nushellTheme} $out/$platform/nushell/generated-theme.nu
    done
  ''
