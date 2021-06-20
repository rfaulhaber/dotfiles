{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    i18n = { defaultLocale = "en_US.UTF-8"; };
    environment.variables = rec {
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

    time = {
      timeZone = "America/New_York";
      hardwareClockInLocalTime = true;
    };
  };
}
