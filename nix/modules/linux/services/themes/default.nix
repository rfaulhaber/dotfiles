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
  # Use base16 library directly without importing the nixosModule.
  # This avoids creating a `scheme` option in the NixOS options tree,
  # which would trigger a warning during documentation generation.
  base16 = pkgs.callPackage inputs.base16.lib {};
in {
  options.modules.themes = {
    active = mkOption {
      type = types.str;
      description = "The active theme.";
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
        message = "${cfg.active} is not a valid theme! For valid themes, see: https://tinted-theming.github.io/base16-gallery";
      }
    ];

    modules.themes.colors = base16.mkSchemeAttrs schemePath;
  };
}
