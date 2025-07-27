{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.sound;
in {
  options.modules.desktop.sound = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    services.pipewire.enable = true;

    environment.systemPackages = with pkgs; [
      pulsemixer
      gnome-bluetooth
    ];
  };
}
