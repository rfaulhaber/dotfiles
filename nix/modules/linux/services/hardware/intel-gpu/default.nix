{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.hardware.intel-gpu;
in {
  options.modules.hardware.intel-gpu = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    hardware.graphics = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # VA-API (iHD) for hardware video transcoding
        intel-compute-runtime # OpenCL runtime for OpenVINO / ML inference
        vpl-gpu-rt # Video Processing Library for modern Intel GPUs
      ];
    };

    environment.sessionVariables = {
      LIBVA_DRIVER_NAME = "iHD";
    };

    user.extraGroups = ["video" "render"];
  };
}
