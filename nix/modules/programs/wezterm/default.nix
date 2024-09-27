{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.wezterm;
in {
  options.modules.programs.wezterm = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [wezterm];

    home.file.wezterm_config = {
      source = "${config.dotfiles.configDir}/wezterm";
      target = "${config.user.home}/.config/wezterm";
    };
  };
}
