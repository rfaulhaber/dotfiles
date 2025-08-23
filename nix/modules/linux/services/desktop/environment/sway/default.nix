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
    modules.desktop = {
      swww.enable = true;
      wayland.enable = true;
      waybar.enable = true;
      environment.type = "wayland";
      fuzzel.enable = true;
    };

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraOptions = ["--unsupported-gpu"];
    };

    services = {
      gnome.gnome-keyring.enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };

    security = {
      # the above uses gdm to login, so we have to also set enableGnomeKeyring here maybe
      pam.services.gdm.enableGnomeKeyring = true;
      polkit.enable = true;
    };

    user.packages = with pkgs; [
      fuzzel
      swaylock
      sway-contrib.grimshot
    ];

    home.file.swayconf = {
      source = "${config.dotfiles.configDir}/sway/config";
      target = "${config.user.home}/.config/sway/config";
    };
  };
}
