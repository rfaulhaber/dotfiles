{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}:
with lib; let
  cfg = config.modules.programs.ghostty;
  colors = config.modules.themes.colors.withHashtag;
  font = "Hack Nerd Font Mono";
in {
  options.modules.programs.ghostty = {
    enable = mkEnableOption false;
    package = mkOption {
      description = "Ghostty package.";
      type = types.package;
      default =
        if isDarwin
        then pkgs.ghostty-bin
        else pkgs.ghostty;
    };
    fontSize = mkOption {
      description = "Font size.";
      type = types.int;
      default = 16;
    };
    extraConfig = mkOption {
      description = "Additional Ghostty configuration lines.";
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    user.packages = [cfg.package];

    home.file = {
      ghosttyConfig = {
        target = "${config.user.home}/.config/ghostty/config";
        text =
          ''
            config-file = theme
            font-family = ${font}
            font-size = ${toString cfg.fontSize}
            window-inherit-working-directory = false
            shell-integration-features = cursor,sudo,title,ssh-env,ssh-terminfo
          ''
          + optionalString (cfg.extraConfig != "") cfg.extraConfig;
      };

      ghosttyTheme = {
        target = "${config.user.home}/.config/ghostty/theme";
        text = ''
          background = ${colors.base00}
          foreground = ${colors.base05}
          cursor-color = ${colors.base05}
          selection-background = ${colors.base02}
          selection-foreground = ${colors.base05}

          palette = 0=${colors.base00}
          palette = 1=${colors.red}
          palette = 2=${colors.green}
          palette = 3=${colors.yellow}
          palette = 4=${colors.blue}
          palette = 5=${colors.magenta}
          palette = 6=${colors.cyan}
          palette = 7=${colors.base05}
          palette = 8=${colors.base03}
          palette = 9=${colors.bright-red}
          palette = 10=${colors.bright-green}
          palette = 11=${colors.base09}
          palette = 12=${colors.bright-blue}
          palette = 13=${colors.bright-magenta}
          palette = 14=${colors.bright-cyan}
          palette = 15=${colors.base07}
        '';
      };

      ghosttyApp = mkIf isDarwin {
        source = "${cfg.package}/Applications/Ghostty.app";
        target = "${config.user.home}/Applications/Ghostty.app";
      };
    };
  };
}
