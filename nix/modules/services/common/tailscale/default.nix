{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.tailscale;
in {
  options.modules.services.tailscale = {
    enable = mkEnableOption false;
    useRoutingFeatures = mkOption {
      description = "Option that forwards services.tailscale.useRoutingFeatures";
      default = "none";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    services.tailscale = {
      enable = true;
    };
  };
}
