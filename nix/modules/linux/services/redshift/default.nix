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
      inherit (config.userInfo.location) latitude longitude;
      provider = "manual";
    };

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
