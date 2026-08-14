# for outputting global static config for use in non-declarative applications
{config, ...}: let
  inherit (builtins) toJSON;
  themeCfg = config.modules.themes;
in {
  config = {
    home.configFile = {
      "globals.json".text = toJSON {
        inherit (themeCfg) font;
        colors.theme = themeCfg.themeAttrs;
        themeFile = themeCfg.colors;
        name = themeCfg.colors.scheme;
        bin = {
          randomWallpaper = {
            logFile = "~/.local/share/random-wallpaper/log.json";
          };
        };
      };
      "theme.scss".text = themeCfg.scss;
    };
  };
}
