# for outputting global static config for use in non-delcarative applications
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
        colors = with colors.withHashtag; {
          theme = {
            inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base0A base0B base0C base0D base0E base0F base10 base11 base12 base13 base14 base15 base16 base17 red green yellow blue cyan magenta;
            background = base00;
            cursorBg = base05;
            cursorBorder = base05;
            cursorFg = base00;
            foreground = base05;
            selectionBg = base05;
            selectionFg = base00;
          };
          themeFile = colors;
          name = colors.scheme;
        };
        bin = {
          randomWallpaper = {
            logFile = "~/.wallpaper-log.json";
          };
        };
      };
    };
  };
}
