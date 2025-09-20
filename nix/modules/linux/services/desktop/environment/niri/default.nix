{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.niri;
in {
  imports = [
    ../../swww
    ../../wayland
  ];

  options.modules.desktop.environment.niri.enable = mkEnableOption false;

  config = mkIf cfg.enable {
    nix.settings = {
      substituters = ["https://niri.cachix.org"];
      trusted-public-keys = ["niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="];
    };

    modules.desktop = {
      swww.enable = true;
      wayland.enable = true;
      waybar.enable = true;
      environment.type = "wayland";
      fuzzel.enable = true;

      # TODO get this to work!
      # autostart.entries = [
      #   "${pkgs.xwayland-satellite}/bin/xwayland-satellite"
      # ];
    };

    security.polkit.enable = true;

    programs = {
      niri = {
        enable = true;
        package = inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.default;
      };

      xwayland.enable = true;
    };

    services = {
      gnome.gnome-keyring.enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };

    # the above uses gdm to login, so we have to also set enableGnomeKeyring here maybe
    security.pam.services.gdm.enableGnomeKeyring = true;

    user.packages = with pkgs; [
      fuzzel
      swaylock
    ];

    environment.systemPackages = with pkgs; [
      xdg-desktop-portal-gtk
      xwayland-satellite
    ];

    home.file.niriconf = {
      source = "${config.dotfiles.configDir}/niri/config.kdl";
      target = "${config.user.home}/.config/niri/config.kdl";
    };
  };
}
