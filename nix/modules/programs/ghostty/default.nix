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
  font = config.modules.themes.font;
  ghosttyConfigs = import ../../../lib/configs/ghostty.nix {
    inherit colors font;
    inherit (cfg) fontSize extraConfig;
  };
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
        text = ghosttyConfigs.config;
      };

      ghosttyTheme = {
        target = "${config.user.home}/.config/ghostty/theme";
        text = ghosttyConfigs.theme;
      };

      ghosttyApp = mkIf isDarwin {
        source = "${cfg.package}/Applications/Ghostty.app";
        target = "${config.user.home}/Applications/Ghostty.app";
      };
    };
  };
}
