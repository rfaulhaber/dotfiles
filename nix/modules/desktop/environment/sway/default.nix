{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.sway;
in {
  imports = [
    ../../wayland
    ../../swww
  ];
  options.modules.desktop.environment.sway = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules = {
      desktop = {
        swww.enable = true;
        wayland.enable = true;
        environment.type = "wayland";
      };
    };

    security.polkit.enable = true;
    services.gnome.gnome-keyring.enable = true;

    # TODO swaylock
    # TODO waybar

    programs = {
      sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraOptions = ["--unsupported-gpu"];
      };
    };

    services.xserver.displayManager = {
      gdm = {
        enable = true;
        wayland = true;
      };
    };

    # the above uses gdm to login, so we have to also set enableGnomeKeyring here maybe
    security.pam.services.gdm.enableGnomeKeyring = true;

    user.packages = with pkgs; [
      wmenu
      sway-contrib.grimshot
    ];

    home.file.swayconf = {
      source = "${config.dotfiles.configDir}/sway/config";
      target = "${config.user.home}/.config/sway/config";
    };
  };
}
