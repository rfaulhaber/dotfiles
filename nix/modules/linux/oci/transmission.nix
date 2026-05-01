{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.transmission;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.transmission = {
    enable = mkEnableOption "Transmission BitTorrent client";

    image = mkOption {
      description = "Transmission container image.";
      type = types.str;
      default = "lscr.io/linuxserver/transmission:latest";
    };

    baseDir = mkOption {
      description = "Base directory for transmission state.";
      type = types.str;
      example = "/data/apps/transmission";
    };

    downloadsDir = mkOption {
      description = "Host directory for completed downloads (mounted at /downloads).";
      type = types.str;
      example = "/data/transmission";
    };

    webPort = mkOption {
      description = "Host port for the transmission web UI.";
      type = types.port;
      default = 9091;
    };

    peerPort = mkOption {
      description = "Peer port (BitTorrent peer connections, tcp + udp).";
      type = types.port;
      default = 51413;
    };

    username = mkOption {
      description = "Transmission RPC username.";
      type = types.str;
      default = "ryan";
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
        gluetun publishes the web/peer ports to the host instead.
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

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    portMappings = [
      "${toString cfg.webPort}:9091"
      "${toString cfg.peerPort}:51413/tcp"
      "${toString cfg.peerPort}:51413/udp"
    ];
    arr = ociLib.mkArrService {
      name = "transmission";
      image = cfg.image;
      baseDir = cfg.baseDir;
      configProperties = cfg.configProperties;
      mediaMounts = ["${cfg.downloadsDir}:/downloads"];
      inherit (cfg) useGluetun gluetunContainer networks user timezone;
      extraEnv = {
        "USER" = cfg.username;
      };
      environmentFiles = [config.sops.templates."transmission-env".path];
      ports = portMappings;
      gluetunPorts = portMappings;
    };
  in {
    sops.secrets."transmission/password" = {};

    sops.templates."transmission-env".content = ''
      TRANSMISSION_PASS=${config.sops.placeholder."transmission/password"}
    '';

    virtualisation.oci-containers.containers.transmission = arr.container;
    systemd.services."podman-transmission" = arr.serviceConfig;
    modules.linux.oci._managedPaths = arr.managedPaths;
    modules.linux.oci._gluetunPorts = arr.gluetunPorts;
    modules.linux.oci.networks = arr.networks;
  });
}
