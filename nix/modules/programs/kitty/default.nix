{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.programs.kitty;
  colors = config.modules.themes.colors;
in {
  options.modules.programs.kitty = { enable = mkEnableOption false; };
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
        color8 = colors.bg-alt;
        color9 = colors.magenta;
        color10 = colors.teal;
        color11 = colors.yellow;
        color12 = colors.bright-blue;
        color13 = colors.magenta;
        color14 = colors.cyan;
        color15 = colors.fg-alt;

        font_size = 20;
      };
    };
  };
}
