{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.wezterm;
  colors = config.modules.themes.colors;
in {
  options.modules.programs.wezterm = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    home.programs.wezterm = {
      enable = true;
      colorSchemes.systemTheme = {
        ansi = with colors; [
          black
          red
          green
          yellow
          blue
          magenta
          cyan
          white
        ];

        brights = with colors; [
          bright-black
          bright-red
          bright-green
          bright-yellow
          bright-blue
          bright-magenta
          bright-cyan
          bright-white
        ];

        background = colors.bg;
        cursor_bg = colors.black;
        cursor_fg = colors.white;
        foreground = colors.fg;
        # cursor_border = colors.grey;
        # selection_bg = "#444444";
        # selection_fg = "#E9E9E9";
      };
      extraConfig = ''
        return {
               color_scheme = "systemTheme",
               font = wezterm.font("Hack Nerd Font Mono"),
               font_size = 20.0,
        }
      '';
    };
  };
}
