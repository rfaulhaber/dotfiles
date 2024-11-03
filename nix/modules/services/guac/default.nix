{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.guac;
in {
  options.modules.services.guac = {
    server = {
      enable = mkEnableOption false;
    };
    enable = mkEnableOption false;
  };

  config = mkMerge [
    (mkIf (cfg.server.enable) {
      services.guacamole-server = {
        enable = true;
      };
    })
  ];
}
