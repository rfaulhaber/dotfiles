{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.immich-ml;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  mlImage = imageLib.mkGpuImage {
    inherit (cfg) image;
    inherit (cfg) gpu;
  };
in {
  options.modules.linux.oci.services.immich-ml = {
    enable = mkEnableOption "Immich machine learning sidecar (standalone)";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/immich-app/immich-machine-learning";
      version = "release";
    };

    gpu = mkOption {
      description = "GPU type for ML inference (null for CPU-only).";
      type = types.nullOr (types.enum ["nvidia" "intel"]);
      default = null;
    };

    port = mkOption {
      description = "Host port to expose the ML service on.";
      type = types.port;
      default = 3003;
    };

    networks = mkOption {
      description = "Networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    openFirewall = mkOption {
      description = "Whether to open firewall port for the ML service.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    modules.linux.oci.volumes.immich_model_cache.enable = true;

    virtualisation.oci-containers.containers."immich_machine_learning" = {
      image = mlImage;
      volumes = [
        "${ociLib.volumeName "immich_model_cache"}:/cache"
      ];
      ports = [
        "${toString cfg.port}:3003"
      ];
      extraOptions =
        ["--network-alias=immich_machine_learning"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ optionals (cfg.gpu == "nvidia") ["--device=nvidia.com/gpu=all"]
        ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri"]
        ++ imageLib.mkImageLabels {
          module = "immich-ml";
          inherit (cfg) image;
        };
      environment = optionalAttrs (cfg.gpu == "nvidia") {
        "NVIDIA_VISIBLE_DEVICES" = "all";
      };
      log-driver = "journald";
    };

    systemd.services."podman-immich_machine_learning" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      volumes = ["immich_model_cache"];
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  };
}
