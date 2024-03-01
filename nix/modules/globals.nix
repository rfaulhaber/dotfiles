{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  home = config.home;
  colors = config.modules.themes.colors;
in {
  config = {
    home.configFile."globals.json" = {
      text = builtins.toJSON {
        inherit colors;
      };
    };
  };
}
