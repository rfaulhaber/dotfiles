{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.nzbget;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.nzbget = {
    enable = mkEnableOption "NZBGet Usenet downloader";

    image = imageLib.mkImageOptions {
      repository = "lscr.io/linuxserver/nzbget";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for nzbget state (mounted at /config).";
      type = types.str;
      example = "/data/apps/nzbget";
    };

    downloadsDir = mkOption {
      description = "Host directory for completed Usenet downloads (mounted at /downloads).";
      type = types.str;
      example = "/data/nzb";
    };

    webPort = mkOption {
      description = "Host port for the nzbget web UI (forwarded onto gluetun when useGluetun = true).";
      type = types.port;
      default = 6789;
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
        gluetun publishes the web port to the host instead. Strongly
        recommended for nzbget since the Usenet server connection
        identifies the account.
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
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = ["${toString cfg.webPort}:6789"];
    arr = ociLib.mkArrService {
      name = "nzbget";
      image = imageLib.renderImage cfg.image;
      extraOptions = imageLib.mkImageLabels {
        module = "nzbget";
        image = cfg.image;
      };
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = ["${cfg.downloadsDir}:/downloads"];
      inherit (cfg) useGluetun gluetunContainer networks user timezone dependsOn;
      ports = portMappings;
      gluetunPorts = portMappings;
    };
  in {
    virtualisation.oci-containers.containers.nzbget = arr.container;
    systemd.services."podman-nzbget" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci._gluetunPorts = arr.gluetunPorts;
    modules.linux.oci.networks = arr.networks;
  });
}
