{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.plex;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.plex = {
    enable = mkEnableOption "Plex media server";

    baseDir = mkOption {
      description = "Base directory for Plex data (config, transcode).";
      type = types.str;
      example = "/data/apps/plex";
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

    useNvidia = mkOption {
      description = "Enable NVIDIA GPU for hardware transcoding.";
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
        description = "UID for the plex user inside container.";
        type = types.int;
        default = config.user.uid;
      };
      gid = mkOption {
        description = "GID for the plex group inside container.";
        type = types.int;
        default = config.user.gid;
      };
    };
  };

  config = mkIf cfg.enable {
    # Ensure the default network exists if we're using it
    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    virtualisation.oci-containers.containers."plex" = {
      image = "linuxserver/plex";
      environment =
        {
          "PGID" = toString cfg.user.gid;
          "PUID" = toString cfg.user.uid;
          "TZ" = cfg.timezone;
          "VERSION" = "docker";
        }
        // optionalAttrs cfg.useNvidia {
          "NVIDIA_VISIBLE_DEVICES" = "all";
        };
      volumes =
        [
          "${cfg.baseDir}/config:/config:rw"
          "${cfg.baseDir}/transcode:/transcode:rw"
        ]
        ++ (mapAttrsToList (name: path: "${path}:/${name}:rw") cfg.mediaDirs);
      ports = [
        "32400:32400/tcp"
        "3005:3005/tcp"
        "8324:8324/tcp"
        "32410:32410/udp"
        "32412:32412/udp"
        "32413:32413/udp"
        "32414:32414/udp"
        "32469:32469/tcp"
      ];
      log-driver = "journald";
      extraOptions =
        ["--network-alias=plex"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ optionals cfg.useNvidia [
          "--device=nvidia.com/gpu=all"
        ];
    };

    systemd.services."podman-plex" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
