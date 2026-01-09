{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.ghostty;
in {
  options.modules.programs.ghostty = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ghostty];

    home.file = {
      ghostty_config_dir = {
        source = "${config.dotfiles.configDir}/ghostty";
        target = "${config.user.home}/.config/ghostty";
        recursive = true;
      };

      ghostty_config_file = {
        target = "${config.user.home}/.config/ghostty/config";
        text = ''
          config-file = ${config.networking.hostName}
        '';
      };
    };
  };
}
