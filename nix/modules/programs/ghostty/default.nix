{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}:
with lib; let
  cfg = config.modules.programs.ghostty;
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
  };

  config = mkIf cfg.enable {
    user.packages = [cfg.package];

    home.file = {
      ghosttyConfigDir = {
        source = "${config.dotfiles.configDir}/ghostty";
        target = "${config.user.home}/.config/ghostty";
        recursive = true;
      };

      ghosttyConfigFile = {
        target = "${config.user.home}/.config/ghostty/config";
        text = ''
          config-file = ${config.networking.hostName}
        '';
      };

      ghosttyApp = mkIf isDarwin {
        source = "${cfg.package}/Applications/Ghostty.app";
        target = "${config.user.home}/Applications/Ghostty.app";
      };
    };
  };
}
