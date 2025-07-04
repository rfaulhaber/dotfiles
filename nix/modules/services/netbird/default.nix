{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.netbird;
in {
  options.modules.services.netbird = {
    enable = mkEnableOption false;
    autoStart = mkOption {
      type = types.bool;
      default = false;
      description = "Whether or not to auto start the default netbird interface.";
    };
  };

  config = mkIf cfg.enable {
    services.netbird = {
      enable = true;
      clients.default.autoStart = cfg.autoStart;
    };

    user.packages = with pkgs;
      [
        netbird
      ]
      ++ lib.optionals config.modules.desktop.enable [
        netbird-ui
      ];
  };
}
