{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.hardware.nvidia;
in {
  options.modules.hardware.nvidia = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    hardware = {
      graphics.enable = true;
      nvidia = {
        modesetting.enable = true;
        open = true;

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
