{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.niri;
  colors = config.modules.themes.colors;
  niriPkg = inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.default;
in {
  imports = [
    ../../swww
    ../../wayland
    inputs.niri-flake.nixosModules.niri
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
    };

    systemd.packages = [niriPkg];

    security.polkit.enable = true;

    programs = {
      niri = {
        enable = true;
        package = niriPkg;
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

    security.pam.services.gdm.enableGnomeKeyring = true;

    user.packages = with pkgs; [
      fuzzel
      swaylock
    ];

    environment.systemPackages = with pkgs; [
      xdg-desktop-portal-gtk
      xwayland-satellite
      nautilus
    ];

    # niri settings via the sodiboo/niri-flake home-manager module
    # (nixosModules.niri auto-imports the HM module when home-manager is present)
    home-manager.users.${config.user.name}.programs.niri.settings = {
      input = {
        keyboard.xkb = {};
        touchpad = {
          tap = true;
          natural-scroll = true;
        };
      };

      outputs."DP-1" = {
        mode = {
          width = 3840;
          height = 2160;
          refresh = 59.997;
        };
        scale = 1;
        transform.rotation = 0;
        position = {
          x = 1280;
          y = 0;
        };
      };

      layout = import ./layout.nix {inherit colors;};

      spawn-at-startup = [
        {argv = ["waybar"];}
        {argv = ["xwayland-satellite"];}
      ];

      environment = {
        DISPLAY = ":0";
      };

      screenshot-path = "~/pictures/screenshots/screenshot-%Y-%m-%d-%H:%M:%S.png";

      window-rules = import ./window-rules.nix;
      binds = import ./binds.nix;
    };
  };
}
