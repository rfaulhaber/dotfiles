{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.netbird;
in {
  options.modules.services.netbird = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    services.netbird.enable = true;
    user.packages = with pkgs;
      [
        netbird
      ]
      ++ lib.optionals config.modules.desktop.enable [
        netbird-ui
      ];
  };
}
