{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.syncthing;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.syncthing = {
    enable = mkEnableOption "Syncthing file synchronization";

    image = imageLib.mkImageOptions {
      repository = "lscr.io/linuxserver/syncthing";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for syncthing state (mounted at /config).";
      type = types.str;
      example = "/data/apps/syncthing";
    };

    syncDirs = mkOption {
      description = ''
        Attrset of additional bind mounts. Keys are paths inside the container
        (without the leading slash); values are host paths. Mounted read-write.
      '';
      type = types.attrsOf types.str;
      default = {};
      example = {
        data = "/data/sync";
        "data/org" = "/data/org";
      };
    };

    webPort = mkOption {
      description = "Host port for the syncthing web UI.";
      type = types.port;
      default = 8384;
    };

    listenPort = mkOption {
      description = "Host port for the syncthing transfer protocol (tcp + udp).";
      type = types.port;
      default = 22000;
    };

    discoveryPort = mkOption {
      description = "Host port for local discovery (udp).";
      type = types.port;
      default = 21027;
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
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    arr = ociLib.mkArrService {
      name = "syncthing";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "syncthing";
        image = cfg.image;
      };
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = mapAttrsToList (mountPoint: hostPath: "${hostPath}:/${mountPoint}:rw") cfg.syncDirs;
      inherit (cfg) networks user timezone dependsOn;
      ports = [
        "${toString cfg.webPort}:8384"
        "${toString cfg.listenPort}:22000/tcp"
        "${toString cfg.listenPort}:22000/udp"
        "${toString cfg.discoveryPort}:21027/udp"
      ];
    };
  in {
    virtualisation.oci-containers.containers.syncthing = arr.container;
    systemd.services."podman-syncthing" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci.networks = arr.networks;
  });
}
