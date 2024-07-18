{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
in {
  imports = [
    ./awesome
    ./bspwm
    ./firefox
    ./i3
    ./lightdm
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
    assertions = let
      desktops = [
        cfg.bspwm.enable
        cfg.awesome.enable
        cfg.i3.enable
      ];
    in [
      {
        assertion = (count (x: x) desktops) == 1;
        message = "You cannot have more than one desktop enabled.";
      }
    ];
    services.xserver = {
      enable = true;
      xkb = {
        options = "eurosign:e";
        layout = "us";
      };
      videoDrivers = mkIf ((length cfg.videoDrivers) > 0) cfg.videoDrivers;
    };

    # TODO make configurable
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        (nerdfonts.override {fonts = ["Hack"];})
        lato
        merriweather
        # corefonts
      ];
      fontconfig.defaultFonts = {
        serif = ["Merriweather"];
        sansSerif = ["Lato"];
        monospace = ["Hack Nerd Font Mono"];
      };
    };

    # sets desktop to dark theme
    programs.dconf.enable = true;
    home.dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };

    # TODO create pdf module
    programs.evince.enable = true;

    # TODO put these somewhere better
    # necessary utilities for desktop
    environment.systemPackages = with pkgs; [
      chromium
      discord
      evince
      gnome-screenshot
      openvpn
      python3
      signal-desktop
      spotify
      tdesktop
    ];
  };
}
