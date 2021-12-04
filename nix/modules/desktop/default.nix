{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop;
in {
  imports = [ ./bspwm ./polybar ./rofi ./util.nix ./random-wallpaper.nix ];
  config = mkIf config.services.xserver.enable {
    i18n = { defaultLocale = "en_US.UTF-8"; };
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
      enableDefaultFonts = true;
      fonts = with pkgs; [
        (nerdfonts.override { fonts = [ "Hack" ]; })
        lato
        merriweather
      ];
      fontconfig.defaultFonts = {
        serif = [ "Merriweather" ];
        sansSerif = [ "Lato" ];
        monospace = [ "Hack Nerd Font Mono" ];
      };
    };

    programs.evince.enable = true;
    programs.seahorse.enable = true;

    security.pam.services.lightdm.enableGnomeKeyring = true;

    # TODO put these somewhere better
    # necessary utilities for desktop
    environment.systemPackages = with pkgs; [
      betterlockscreen
      chromium
      discord
      evince
      feh
      firefox-devedition-bin
      gnome3.gnome-screenshot
      keychain
      libsecret
      openvpn
      pass
      python3
      spotify
      tdesktop
      wally-cli
      xscreensaver
      xtitle
    ];
  };
}
