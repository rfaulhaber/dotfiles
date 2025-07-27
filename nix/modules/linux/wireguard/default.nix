{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.wireguard;
in {
  options.modules.services.wireguard = {
    enable = mkEnableOption false;
    name = mkOption {
      description = "Wireguard interface name.";
      type = types.str;
      default = "wg0";
    };
    config = mkOption {
      description = "Path to Wireguard config.";
      type = types.either types.str types.path;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [wireguard-tools];

    systemd.network.netdevs = mkIf (cfg.name != null && cfg.config != null) {
      ${cfg.name} = {
        wireguardConfig = cfg.config;
      };
    };
  };
}
