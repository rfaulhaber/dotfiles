{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      # Experimental exposes device battery levels over D-Bus (headphones etc.)
      settings.General.Experimental = true;
    };
  };
}
