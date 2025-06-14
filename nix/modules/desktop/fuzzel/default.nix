{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.fuzzel;
in {
  options.modules.desktop.fuzzel = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      fuzzel
    ];

    # TODO generate from config
    home.file.fuzzel = {
      source = "${config.dotfiles.configDir}/fuzzel/fuzzel.ini";
      target = "${config.user.home}/.config/fuzzel/fuzzel.ini";
    };
  };
}
