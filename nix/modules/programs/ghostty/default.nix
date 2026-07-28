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
    startWithZellij = {
      enable = mkOption {
        description = "Starts new instances in Zellij";
        type = types.bool;
        default = false;
      };
      enableWelcomeScreen = mkOption {
        description = "If enabled, starts new instaces of Zellij in the welcome screen";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.startWithZellij.enable -> config.modules.programs.zellij.enable;
        message = "Zellij must be enabled to use the `startWithZellij` option";
      }
    ];
    user.packages = [cfg.package];

    home.file = {
      ghosttyConfig = {
        target = "${config.user.home}/.config/ghostty/config";
        text = let
          zellijFlags = "-l welcome";
          zellijInit = "${pkgs.zellij}/bin/zellij";
          zellijCmd = zellijInit + lib.optionalString cfg.startWithZellij.enableWelcomeScreen zellijFlags;
          zellijCommandSetting = "command = ${zellijCmd}";
        in
          ghosttyConfigs.config
          + lib.optionalString cfg.startWithZellij.enable zellijCommandSetting;
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
