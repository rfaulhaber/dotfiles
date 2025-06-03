# for outputting global static config for use in non-delcarative applications
{
  config,
  lib,
  ...
}:
with lib; let
  colors = config.modules.themes.colors;
  font = "Hack Nerd Font Mono";
  theme = with colors.withHashtag; {
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base0A base0B base0C base0D base0E base0F base10 base11 base12 base13 base14 base15 base16 base17 red green yellow blue cyan magenta;
    background = base00;
    cursorBg = base05;
    cursorBorder = base05;
    cursorFg = base00;
    foreground = base05;
    selectionBg = base05;
    selectionFg = base00;
    textColor = base07;
  };
in {
  config = {
    home.configFile = {
      "globals.json".text = builtins.toJSON {
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
        |> lib.attrsToList
        |> (map ({
          name,
          value,
        }: "\$${name}: ${value};"))
        |> (builtins.concatStringsSep
          "\n");
    };
  };
}
