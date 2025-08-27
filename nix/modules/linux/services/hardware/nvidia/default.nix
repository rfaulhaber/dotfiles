{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.nvidia;
in {
  options.modules.hardware.nvidia = {
    enable = mkEnableOption false;
    useOpenDrivers = mkOption {
      type = types.bool;
      description = "Whether or not to use the open source NVIDIA drivers.";
      default = false;
    };
  };

  config = mkIf cfg.enable {
    hardware = {
      graphics.enable = true;
      nvidia = {
        modesetting.enable = true;
        open = cfg.useOpenDrivers;

        powerManagement = {
          enable = false;
          finegrained = false;
        };
      };
    };

    environment = {
      sessionVariables = {
        GBM_BACKEND = "nvidia-drm";
        LIBVA_DRIVER_NAME = "nvidia";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
    };

    user = {
      extraGroups = ["video"];
      packages = with pkgs; [nvitop];
    };

    services.xserver.videoDrivers = ["nvidia"];
  };
}
