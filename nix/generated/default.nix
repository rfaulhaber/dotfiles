# Builds generated config files for both linux and darwin platforms.
# Output structure:
#   $out/linux/ghostty/{config,theme}
#   $out/linux/nushell/{config.nu,env.nu,generated-theme.nu}
#   $out/linux/zellij/config.kdl
#   $out/darwin/ghostty/{config,theme}
#   $out/darwin/nushell/{config.nu,env.nu,generated-theme.nu}
#   $out/darwin/zellij/config.kdl
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
    inherit (defaults) font;
    fontSize = defaults.ghostty.fontSize;
    extraConfig = ''
      command = /opt/homebrew/bin/nu --login
    '';
  };

  nushellConfigs = import ../lib/configs/nushell.nix {
    colors = colors.withHashtag;
    themeName = colors.scheme;
    # Source the static config from a content-addressed store path instead of a
    # manually-cloned ~/.config/dotfiles. Makes generated-configs self-contained
    # on non-NixOS nix hosts (e.g. a work macOS without nix-darwin) and resolves
    # to the same `nushell-config` store path the home-manager hosts use.
    dotfilesConfigDir = builtins.path {
      path = ../../config/nushell;
      name = "nushell-config";
    };
  };

  zellijConfigs = import ../lib/configs/zellij.nix {
    colors = colors.withHashtag;
    # The managed hosts pin the store path of nu here; outside Nix there is no
    # store, so pin the bare name — still overriding zellij's $SHELL fallback,
    # which dev shells pollute with their own bash.
    defaultShell = "nu";
  };

  ghosttyConfig = pkgs.writeText "ghostty-config" ghosttyConfigs.config;
  ghosttyTheme = pkgs.writeText "ghostty-theme" ghosttyConfigs.theme;
  nushellConfig = pkgs.writeText "nushell-config" nushellConfigs.config;
  nushellEnv = pkgs.writeText "nushell-env" nushellConfigs.env;
  nushellTheme = pkgs.writeText "nushell-generated-theme" nushellConfigs.generated-theme;
  zellijConfig = pkgs.writeText "zellij-config" zellijConfigs.config;
in
  pkgs.runCommand "generated-configs" {} ''
    for platform in linux darwin; do
      mkdir -p $out/$platform/ghostty $out/$platform/nushell $out/$platform/zellij
      cp ${ghosttyConfig} $out/$platform/ghostty/config
      cp ${ghosttyTheme} $out/$platform/ghostty/theme
      cp ${nushellConfig} $out/$platform/nushell/config.nu
      cp ${nushellEnv} $out/$platform/nushell/env.nu
      cp ${nushellTheme} $out/$platform/nushell/generated-theme.nu
      cp ${zellijConfig} $out/$platform/zellij/config.kdl
    done
  ''
