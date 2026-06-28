{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.airvpn;
in {
  options.modules.services.airvpn = {
    enable = mkEnableOption "AirVPN client (Eddie)";
  };

  config = mkIf cfg.enable {
    user.packages = [pkgs.eddie];

    # WireGuard/OpenVPN backends both need the tun device available.
    boot.kernelModules = ["tun"];
  };
}
