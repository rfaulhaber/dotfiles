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
    ./polybar
    ./random-wallpaper.nix
    ./rofi
    ./sound
    ./util.nix
  ];
  options.modules.desktop = {
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
  config = mkIf config.services.xserver.enable {
    environment.variables = rec {
      # TODO move elsewhere
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
      RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
      CARGO_HOME = "${XDG_DATA_HOME}/cargo";
    };

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
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
