{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.navidrome;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.navidrome = {
    enable = mkEnableOption "Navidrome music streaming server";

    image = imageLib.mkImageOptions {
      repository = "deluan/navidrome";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Base directory for navidrome state (mounted at /data inside the
        container — navidrome's layout is rooted at /data, not /config).
      '';
      type = types.str;
      example = "/data/apps/navidrome";
    };

    musicDir = mkOption {
      description = "Host directory holding the music library (mounted read-only at /music).";
      type = types.str;
      example = "/data/music";
    };

    webPort = mkOption {
      description = "Host port for the navidrome web UI / Subsonic API.";
      type = types.port;
      default = 4533;
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as (navidrome uses --user, not PUID).";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID to run the container as.";
        type = types.int;
        default = 100;
      };
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

    extraEnv = mkOption {
      description = "Additional ND_* environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    lastfm.enable = mkEnableOption ''
      last.fm scrobbling and metadata. When true the module requires sops
      secrets at "navidrome/lastfm-api-key" and "navidrome/lastfm-secret"
    '';

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets = mkIf cfg.lastfm.enable {
      "navidrome/lastfm-api-key" = {};
      "navidrome/lastfm-secret" = {};
    };

    sops.templates = mkIf cfg.lastfm.enable {
      "navidrome-env".content = ''
        ND_LASTFM_APIKEY=${config.sops.placeholder."navidrome/lastfm-api-key"}
        ND_LASTFM_SECRET=${config.sops.placeholder."navidrome/lastfm-secret"}
      '';
    };

    virtualisation.oci-containers.containers.navidrome = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          ND_LASTFM_SCROBBLEFIRSTARTISTONLY = "true";
        }
        // cfg.extraEnv;
      environmentFiles = optional cfg.lastfm.enable config.sops.templates."navidrome-env".path;
      volumes = [
        "${cfg.baseDir}:/data"
        "${cfg.musicDir}:/music:ro"
      ];
      ports = ["${toString cfg.webPort}:4533"];
      extraOptions =
        ["--network-alias=navidrome"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ ["--user=${toString cfg.user.uid}:${toString cfg.user.gid}"]
        ++ imageLib.mkImageLabels {
          module = "navidrome";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-navidrome" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      sopsTemplates = optional cfg.lastfm.enable "navidrome-env";
    };
  };
}
