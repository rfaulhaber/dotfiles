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
      extraPackages = with pkgs; [
        xdg-desktop-portal-gtk
        xwayland-satellite
      ];
    };

    services = {
      gnome.gnome-keyring.enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };

    security = {
      pam.services = {
        # the above uses gdm to login, so we have to also set enableGnomeKeyring here maybe
        gdm.enableGnomeKeyring = true;
        sway = {
          # can I do this? will it work?
          enableGnomeKeyring = true;
          gnupg.enable = true;
        };
      };

      polkit.enable = true;
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
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
