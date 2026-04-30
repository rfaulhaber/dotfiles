{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.sonarr;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.sonarr = {
    enable = mkEnableOption "Sonarr TV collection manager";

    image = mkOption {
      description = "Sonarr container image.";
      type = types.str;
      default = "lscr.io/linuxserver/sonarr:latest";
    };

    baseDir = mkOption {
      description = "Base directory for sonarr state (mounted at /config).";
      type = types.str;
      example = "/data/apps/sonarr";
    };

    webPort = mkOption {
      description = "Host port for the sonarr web UI (forwarded onto gluetun when useGluetun = true).";
      type = types.port;
      default = 8989;
    };

    mediaDirs = mkOption {
      description = ''
        Attrset of additional bind mounts. Keys are paths inside the container
        (without the leading slash); values are host paths. Mounted read-write.
      '';
      type = types.attrsOf types.str;
      default = {};
      example = {
        tv = "/data/tv";
        "downloads/transmission" = "/data/transmission";
        "downloads/nzb" = "/data/nzb";
      };
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

    useGluetun = mkOption {
      description = ''
        Route all traffic through the gluetun VPN container by joining its
        network namespace. Disables this container's own port mappings;
        gluetun publishes the web port to the host instead.
      '';
      type = types.bool;
      default = false;
    };

    gluetunContainer = mkOption {
      description = "Name of the gluetun container to share netns with.";
      type = types.str;
      default = "gluetun";
    };

    networks = mkOption {
      description = "Networks to join (only used when useGluetun = false).";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
      example = ["prowlarr" "transmission"];
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:8989"];
    arr = ociLib.mkArrService {
      name = "sonarr";
      image = cfg.image;
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = mapAttrsToList (mountPoint: hostPath: "${hostPath}:/${mountPoint}:rw") cfg.mediaDirs;
      inherit (cfg) useGluetun gluetunContainer networks user timezone dependsOn;
      ports = portMappings;
      gluetunPorts = portMappings;
    };
  in {
    virtualisation.oci-containers.containers.sonarr = arr.container;
    systemd.services."podman-sonarr" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci._gluetunPorts = arr.gluetunPorts;
    modules.linux.oci.networks = arr.networks;
  });
}
