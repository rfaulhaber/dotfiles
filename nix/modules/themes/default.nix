{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.themes;
  schemePath = "${inputs.tt-schemes}/base16/${cfg.active}.yaml";
  resolveTheme = import ../../lib/configs/theme.nix {
    inherit pkgs inputs lib;
    themesDir = ./.;
  };
in {
  options.modules.themes = {
    active = mkOption {
      type = types.str;
      description = "The active theme.";
    };

    font = mkOption {
      type = types.str;
      description = "The system-wide font family.";
      default = "Hack Nerd Font Mono";
    };

    colors = mkOption {
      type = types.attrs;
      description = "Active color set.";
      default = {};
    };
  };
  config = {
    assertions = [
      {
        assertion = builtins.pathExists schemePath;
        message = "${cfg.active} is not a valid theme! For valid themes, see: https://tinted-theming.github.io/tinted-gallery/";
      }
    ];

    modules.themes.colors = resolveTheme {themeName = cfg.active;};
  };
}
