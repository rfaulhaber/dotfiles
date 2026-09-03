{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.nvtop;
  nvidiaEnabled = config.modules.hardware.nvidia.enable;
  intelEnabled = config.modules.hardware.intel-gpu.enable;
in {
  options.modules.programs.nvtop = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = nvidiaEnabled || intelEnabled;
        message = "A supported GPU must be enabled to enable nvtop";
      }
    ];
    user.packages =
      lib.optionals nvidiaEnabled [pkgs.nvtopPackages.nvidia]
      ++ lib.optionals intelEnabled [pkgs.nvtopPackages.intel];
  };
}
