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
    package = mkOption {
      type = types.nullOr types.package;
      description = "The NVIDIA driver package to use. When null, uses the default (stable) driver.";
      default = null;
    };
  };

  config = mkIf cfg.enable {
    hardware = {
      graphics.enable = true;
      nvidia = {
        modesetting.enable = true;
        open = cfg.useOpenDrivers;
        package = mkIf (cfg.package != null) cfg.package;

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
