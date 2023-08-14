{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib; let
  cfg = config.modules.desktop;
in {
  imports = [
    ./bspwm
    ./firefox
    ./hyprland
    ./polybar
    ./random-wallpaper.nix
    ./rofi
    ./sound
    ./util.nix
  ];
  options.modules.desktop = {
    # TODO rewrite such that you don't need this
    enable = mkEnableOption false;
    videoDrivers = mkOption {
      description = "Passthrough property for services.xserver.videoDrivers. All desktop configurations use xserver at the moment.";
      type = types.listOf types.str;
      default = [];
    };
    useLaptopSettings = mkOption {
      description = "If enabled, turns on various laptop settings, such as the battery module in Polybar.";
      type = types.bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      videoDrivers = mkIf ((length cfg.videoDrivers) > 0) cfg.videoDrivers;
    };

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        (nerdfonts.override {fonts = ["Hack"];})
        lato
        merriweather
        corefonts
      ];
      fontconfig.defaultFonts = {
        serif = ["Merriweather"];
        sansSerif = ["Lato"];
        monospace = ["Hack Nerd Font Mono"];
      };
    };

    programs.evince.enable = true;

    # TODO put these somewhere better
    # necessary utilities for desktop
    environment.systemPackages = with pkgs; [
      chromium
      discord
      evince
      feh
      gnome.gnome-screenshot
      openvpn
      python3
      spotify
      tdesktop
      wally-cli
    ];
  };
}
