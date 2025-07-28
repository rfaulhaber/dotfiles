{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
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

    virtualisation.docker = {
      enable = true;
      daemon.settings.features.cdi = mkIf cfg.enableNvidiaTools true;
    };

    hardware.nvidia-container-toolkit.enable = mkIf cfg.enableNvidiaTools true;

    environment.systemPackages = with pkgs;
      [
        docker
        docker-compose
      ]
      ++ lib.optionals cfg.enableNvidiaTools (with pkgs; [
        nvidia-docker
        nvidia-container-toolkit
        libnvidia-container
      ]);

    user.extraGroups = ["docker"];
  };
}
