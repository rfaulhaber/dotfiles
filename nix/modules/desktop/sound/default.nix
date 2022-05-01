{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.sound;
in {
  options.modules.desktop.sound = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };

    environment.systemPackages = with pkgs; [
      pulsemixer
      gnome3.gnome-bluetooth
    ];
  };
}
