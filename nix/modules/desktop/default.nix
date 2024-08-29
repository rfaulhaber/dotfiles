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
    ./environment
    ./firefox
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
    monitors = mkOption {
      description = "List of monitors.";
      type = types.listOf types.str;
      default = [];
    };
    videoDrivers = mkOption {
      description = "Passthrough property for services.xserver.videoDrivers.";
      type = types.listOf types.str;
      default = []; # TODO change
    };
    laptop = mkOption {
      description = "Laptop-specific settings.";
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            description = "Enables laptop settings.";
            default = false;
          };
        };
      };
      default = {enable = false;};
    };
  };

  config = mkIf cfg.enable {
    assertions = let
      foldPred = acc: item:
        if item.value.enable
        then acc ++ [item.name]
        else acc;
      desktopsEnabled = foldl foldPred [] (attrsToList config.modules.desktop.environment);
    in [
      {
        assertion = (length desktopsEnabled) == 1;
        message = "You must have one desktop environment selected if the desktop module is enabled. You have ${toString (length desktopsEnabled)} (${toString desktopsEnabled})";
      }
    ];

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
