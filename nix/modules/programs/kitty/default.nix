{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.programs.kitty;
  colors = config.modules.themes.colors;
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
      settings = {
        # colors
        background = colors.bg;
        foreground = colors.fg;

        # normal
        # black
        color0 = colors.black;
        # red
        color1 = colors.red;
        # green
        color2 = colors.green;
        # yellow
        color3 = colors.yellow;
        # blue
        color4 = colors.blue;
        # magenta
        color5 = colors.magenta;
        # cyan
        color6 = colors.cyan;
        # white
        color7 = colors.white;

        # bright
        # black
        color8 = colors.bright-black;
        # red
        color9 = colors.bright-red;
        # green
        color10 = colors.bright-green;
        # yellow
        color11 = colors.bright-yellow;
        # blue
        color12 = colors.bright-blue;
        # magenta
        color13 = colors.bright-magenta;
        # cyan
        color14 = colors.bright-cyan;
        # white
        color15 = colors.bright-white;

        font_size = cfg.fontSize;
      };
    };
  };
}
