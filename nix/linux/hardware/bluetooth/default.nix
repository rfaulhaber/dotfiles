{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = { enable = mkEnableOption false; };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      settings.General.Enable = "Source,Sink,Media,Socket";
    };

    services.blueman.enable = true;

    environment.systemPackages = with pkgs; [ gnome3.gnome-bluetooth ];
  };
}
