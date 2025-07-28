{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.mullvad;
in {
  options.modules.services.mullvad = {
    enable = mkEnableOption false;
    enableGUI = mkOption {
      description = "If enabled, installs the GUI as well.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.mullvad-vpn = {
      enable = true;
      package =
        if cfg.enableGUI
        then pkgs.mullvad-vpn
        else pkgs.mullvad;
    };
  };
}
