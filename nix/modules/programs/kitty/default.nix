{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.kitty;
  colors = config.modules.themes.colors.withHashtag;
in {
  options.modules.programs.kitty = {
    enable = mkEnableOption false;
    fontSize = mkOption {
      description = "Font size.";
      default = 20;
      type = types.int;
      example = 20;
    };
  };
  config = mkIf cfg.enable {
    home.programs.kitty = {
      enable = true;
      settings = with colors; {
        # colors
        background = base00;
        foreground = base07;

        # normal
        # black
        color0 = base00;
        # red
        color1 = red;
        # green
        color2 = green;
        # yellow
        color3 = yellow;
        # blue
        color4 = blue;
        # magenta
        color5 = magenta;
        # cyan
        color6 = cyan;
        # white
        color7 = base07;

        # bright
        # black
        color8 = base08;
        # red
        color9 = bright-red;
        # green
        color10 = bright-green;
        # yellow
        color11 = base09;
        # blue
        color12 = bright-blue;
        # magenta
        color13 = bright-magenta;
        # cyan
        color14 = bright-cyan;
        # white
        color15 = base0F;

        font_size = cfg.fontSize;
      };
    };
  };
}
