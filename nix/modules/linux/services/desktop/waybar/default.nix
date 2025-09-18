{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.waybar;
in {
  options.modules.desktop.waybar = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = [
      inputs.waybar.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];

    home.file.waybarconf = {
      source = "${config.dotfiles.configDir}/waybar";
      target = "${config.user.home}/.config/waybar";
      recursive = true;
    };

    # TODO get this to work!
    # modules.desktop.autostart.entries = [
    #   "${pkgs.waybar}/bin/waybar"
    # ];
  };
}
