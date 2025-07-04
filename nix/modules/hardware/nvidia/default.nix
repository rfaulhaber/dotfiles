{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.nvidia;
  desktopCfg = config.modules.desktop;
in {
  options.modules.hardware.nvidia = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    hardware = {
      graphics.enable = true;
      nvidia = {
        modesetting.enable = true;
        # don't want to use the open source driver... yet
        open = false;

        powerManagement = {
          enable = false;
          finegrained = false;
        };

        package = mkDefault config.boot.kernelPackages.nvidiaPackages.production;
      };
    };

    user.extraGroups = ["video"];

    services.xserver.videoDrivers = ["nvidia"];
  };
}
