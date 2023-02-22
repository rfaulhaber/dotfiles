{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.redshift;
in {
  options.modules.services.redshift = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    location = {
      provider = "manual";
      latitude = 41.45866;
      longitude = -81.787132;
    };

    environment.systemPackages = with pkgs; [redshift];

    services.redshift = {
      enable = true;
      brightness = {
        day = "1";
        night = "1";
      };
      temperature = {
        day = 5700;
        night = 2600;
      };
    };
  };
}
