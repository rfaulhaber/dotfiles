{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.jellyfin;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.jellyfin = {
    enable = mkEnableOption "Jellyfin media server";

    image = mkOption {
      description = "Jellyfin container image.";
      type = types.str;
      default = "lscr.io/linuxserver/jellyfin:latest";
    };

    baseDir = mkOption {
      description = "Base directory for Jellyfin data (config, cache).";
      type = types.str;
      example = "/data/apps/jellyfin";
    };

    tvDir = mkOption {
      description = "Path to tv directory";
      type = types.str;
      default = null;
    };

    moviesDir = mkOption {
      description = "Path to movies directory.";
      type = types.str;
      default = null;
    };

    networks = mkOption {
      description = "Networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    gpu = mkOption {
      description = "GPU type for hardware transcoding (null to disable).";
      type = types.nullOr (types.enum ["nvidia" "intel"]);
      default = null;
    };

    openFirewall = mkOption {
      description = "Whether to open firewall ports for Jellyfin.";
      type = types.bool;
      default = false;
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    user = {
      uid = mkOption {
        description = "UID for the jellyfin user inside container.";
        type = types.int;
        default = config.user.uid;
      };
      gid = mkOption {
        description = "GID for the jellyfin group inside container.";
        type = types.int;
        default = 100;
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    virtualisation.oci-containers.containers."jellyfin" = {
      image = cfg.image;
      environment =
        {
          "PGID" = toString cfg.user.gid;
          "PUID" = toString cfg.user.uid;
          "TZ" = cfg.timezone;
        }
        // optionalAttrs (cfg.gpu == "nvidia") {
          "NVIDIA_VISIBLE_DEVICES" = "all";
        };
      volumes =
        [
          "${cfg.baseDir}/config:/config:rw"
          "${cfg.baseDir}/cache:/cache:rw"
        ]
        ++ lib.optionals (cfg.tvDir != null) [
          "${cfg.tvDir}:/data/tvshows:rw"
        ]
        ++ lib.optionals (cfg.moviesDir != null) [
          "${cfg.moviesDir}:/data/movies:rw"
        ];
      ports = [
        "8096:8096/tcp"
        "8920:8920/tcp"
        "7359:7359/udp"
        "1900:1900/udp"
      ];
      log-driver = "journald";
      extraOptions =
        ["--network-alias=jellyfin"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ optionals (cfg.gpu == "nvidia") ["--device=nvidia.com/gpu=all"]
        ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri:/dev/dri"];
    };

    systemd.services."podman-jellyfin" = mkMerge [
      (ociLib.mkServiceConfig {
        networks = cfg.networks;
      })
      {
        serviceConfig.ExecStartPre = ["${pkgs.coreutils}/bin/mkdir -p ${cfg.baseDir}/config ${cfg.baseDir}/cache"];
      }
    ];

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [8096 8920];
      allowedUDPPorts = [7359 1900];
    };
  };
}
