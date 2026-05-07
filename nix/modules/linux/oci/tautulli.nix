{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.tautulli;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.tautulli = {
    enable = mkEnableOption "Tautulli Plex statistics monitor";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/tautulli/tautulli";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for tautulli state (mounted at /config).";
      type = types.str;
      example = "/data/apps/tautulli";
    };

    webPort = mkOption {
      description = "Host port for the tautulli web UI.";
      type = types.port;
      default = 8181;
    };

    user = {
      uid = mkOption {
        description = "UID inside the container.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID inside the container.";
        type = types.int;
        default = 100;
      };
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    networks = mkOption {
      description = "Networks to join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    arr = ociLib.mkArrService {
      name = "tautulli";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "tautulli";
        image = cfg.image;
      };
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      inherit (cfg) networks user timezone dependsOn;
      ports = ["${toString cfg.webPort}:8181"];
    };
  in {
    virtualisation.oci-containers.containers.tautulli = arr.container;
    systemd.services."podman-tautulli" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci.networks = arr.networks;
  });
}
