{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.themes;
in {
  imports = [inputs.base16.nixosModule];
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
  config = let
    schemePath = "${inputs.tt-schemes}/base16/${cfg.active}.yaml";
  in {
    assertions = [
      {
        assertion = builtins.pathExists schemePath;
        message = "${cfg.active} is not a valid theme! For valid themes, see: https://tinted-theming.github.io/base16-gallery";
      }
    ];

    scheme = schemePath;

    # TODO this is messy, do something else here
    modules.themes.colors = config.scheme;
  };
}
