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

        package = config.boot.kernelPackages.nvidiaPackages.stable;
        # package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
        #   version = "570.172.08";
        #   sha256_64bit = "sha256-AlaGfggsr5PXsl+nyOabMWBiqcbHLG4ij617I4xvoX0=";
        #   sha256_aarch64 = lib.fakeSha256;
        #   openSha256 = lib.fakeSha256;
        #   settingsSha256 = "sha256-N/1Ra8Teq93U3T898ImAT2DceHjDHZL1DuriJeTYEa4=";
        #   persistencedSha256 = lib.fakeSha256;
        # };
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
