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

    mediaDirs = mkOption {
      description = "Attrset of media directories to mount. Keys become mount points inside container.";
      type = types.attrsOf types.str;
      default = {};
      example = {
        movies = "/data/movies";
        tv = "/data/tv";
        music = "/data/music";
      };
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
        ++ (mapAttrsToList (name: path: "${path}:/${name}:rw") cfg.mediaDirs);
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
        ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri"];
    };

    systemd.services."podman-jellyfin" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
