# for outputting global static config for use in non-delcarative applications
{
  config,
  lib,
  ...
}: let
  inherit (builtins) toJSON concatStringsSep map;
  inherit (lib) attrsToList;
  colors = config.modules.themes.colors;
  font = "Hack Nerd Font Mono";
  theme = let
    c = colors.withHashtag;
  in {
    inherit (c) base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base0A base0B base0C base0D base0E base0F base10 base11 base12 base13 base14 base15 base16 base17 red green yellow blue cyan magenta;
    background = c.base00;
    cursorBg = c.base05;
    cursorBorder = c.base05;
    cursorFg = c.base00;
    foreground = c.base05;
    selectionBg = c.base05;
    selectionFg = c.base00;
    textColor = c.base07;
  };
in {
  config = {
    home.configFile = {
      "globals.json".text = toJSON {
        inherit font;
        colors.theme = theme;
        themeFile = colors;
        name = colors.scheme;
        bin = {
          randomWallpaper = {
            logFile = "~/.wallpaper-log.json";
          };
        };
      };
      "theme.scss".text =
        theme
        |> attrsToList
        |> (map ({
          name,
          value,
        }: "\$${name}: ${value};"))
        |> (concatStringsSep "\n");
    };
  };
}
