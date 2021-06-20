{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware = {
      pulseaudio = {
        enable = true;
        extraModules = [ pkgs.pulseaudio-modules-bt ];
        package = pkgs.pulseaudioFull;
      };
      bluetooth = {
        enable = true;
        settings.General.Enable = "Source,Sink,Media,Socket";
      };
    };

    services.blueman.enable = true;

    environment.systemPackages = with pkgs; [
      pulsemixer
      pavucontrol
      gnome3.gnome-bluetooth
    ];
  };
}
