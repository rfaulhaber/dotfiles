{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.podman;
in {
  options.modules.services.podman = {
    enable = mkEnableOption false;
    rootless = mkOption {
      description = "Installs rootless Docker.";
      type = types.bool;
      default = false;
    };
    enableNvidiaTools = mkOption {
      description = "Installs NVIDIA container runtime and toolkit. Must have NVIDIA enabled.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.enableNvidiaTools || config.modules.hardware.nvidia.enable;
        message = "Cannot use NVIDIA tools with Docker if NVIDIA hardware module isn't enabled.";
      }
    ];

    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };

    hardware.nvidia-container-toolkit.enable = mkIf cfg.enableNvidiaTools true;

    user.extraGroups = ["podman"];
  };
}
