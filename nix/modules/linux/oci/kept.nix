{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.kept;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.kept = {
    enable = mkEnableOption "Kept self-hosted notes";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/ericerkz/kept";
      version = "1.6.0";
    };

    baseDir = mkOption {
      description = ''
        Base directory for all Kept state, mounted at /app/data. Upstream
        keeps the SQLite DB, uploads/, attachments/, backups/ and the
        generated vapid.json under this one tree, so a single dataset
        covers the service.
      '';
      type = types.str;
      example = "/data/apps/kept";
    };

    webPort = mkOption {
      description = ''
        Port for the Kept web UI — used for both the host publish and the
        container's internal listen, which Kept takes from PORT. The
        image's own default of 6767 is already claimed on the host side by
        bazarr, so this deliberately differs from upstream's compose.
      '';
      type = types.port;
      default = 8476;
    };

    user = {
      uid = mkOption {
        description = "UID the entrypoint drops to via su-exec, and owner of baseDir.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID the entrypoint drops to via su-exec, and group of baseDir.";
        type = types.int;
        default = 100;
      };
    };

    skipChown = mkOption {
      description = ''
        Set KEPT_SKIP_CHOWN=1. The image starts as root and recursively
        chowns /app/data to PUID:PGID on every start before dropping
        privileges. Cheap for a note store, but worth skipping once
        ownership has settled and the attachment tree has grown large.
      '';
      type = types.bool;
      default = false;
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

    extraEnv = mkOption {
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {};
      example = {KEPT_SESSION_TTL_DAYS = "14";};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    # ----- application settings ---------------------------------------
    # Kept has no config file: every knob below is an environment
    # variable read directly by server/server.js.

    baseUrl = mkOption {
      description = ''
        Public URL Kept believes it is served at, used to build OAuth
        redirect URIs for the optional Google Calendar integration. Null
        derives it per-request, which is correct unless a reverse proxy
        rewrites the path or drops X-Forwarded-Proto.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "https://kept.example.com";
    };

    corsOrigins = mkOption {
      description = ''
        Origins allowed to make browser API calls. Empty means same-origin
        only. Takes precedence over corsAllowAll when both are set.
      '';
      type = types.listOf types.str;
      default = [];
      example = ["https://kept.example.com"];
    };

    corsAllowAll = mkOption {
      description = ''
        Send Access-Control-Allow-Origin: *. Upstream's compose ships this
        enabled; it is off here because it exposes the unauthenticated
        endpoints (/api/setup/status, login, register) to script on any
        origin. Authenticated calls still require a bearer token.
      '';
      type = types.bool;
      default = false;
    };

    allowRestore = mkOption {
      description = ''
        Enable POST /api/setup/restore, which overwrites the database from
        an uploaded SQLite backup. Any holder of a valid auth token can
        invoke it, so leave this off outside an actual restore.
      '';
      type = types.bool;
      default = false;
    };

    linkPreviewScreenshots = mkOption {
      description = ''
        Let Kept fetch link-preview thumbnails from image.thum.io, which
        discloses every URL pasted into a note to a third party. Upstream
        leaves this on; it is off here.
      '';
      type = types.bool;
      default = false;
    };

    vapid = {
      enable = mkEnableOption ''
        pinning the web-push VAPID keypair from sops instead of letting
        Kept generate one into baseDir/vapid.json on first run. Pinned keys
        survive a restore onto an empty volume, so existing browser push
        subscriptions keep working. Kept only honours the pair when both
        halves are present. Requires sops secrets at "kept/vapid-public-key"
        and "kept/vapid-private-key"
      '';

      subject = mkOption {
        description = "VAPID JWT subject — a mailto: or https: URL identifying the sender.";
        type = types.str;
        example = "https://kept.example.com";
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets = optionalAttrs cfg.vapid.enable {
      "kept/vapid-public-key" = {};
      "kept/vapid-private-key" = {};
    };

    sops.templates = optionalAttrs cfg.vapid.enable {
      "kept-vapid-env".content = ''
        VAPID_PUBLIC_KEY=${config.sops.placeholder."kept/vapid-public-key"}
        VAPID_PRIVATE_KEY=${config.sops.placeholder."kept/vapid-private-key"}
        VAPID_SUBJECT=${cfg.vapid.subject}
      '';
    };

    virtualisation.oci-containers.containers.kept = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "PUID" = toString cfg.user.uid;
          "PGID" = toString cfg.user.gid;
          "TZ" = cfg.timezone;
          "PORT" = toString cfg.webPort;
          # Pins the DB inside the bind mount explicitly. The sibling
          # uploads/, attachments/ and backups/ trees still follow Kept's
          # own dataDir, which it resolves relative to server/server.js.
          "SQLITE_PATH" = "/app/data/kept.sqlite";
        }
        // optionalAttrs cfg.skipChown {"KEPT_SKIP_CHOWN" = "1";}
        // optionalAttrs (cfg.baseUrl != null) {"BASE_URL" = cfg.baseUrl;}
        // optionalAttrs (cfg.corsOrigins != []) {
          "KEPT_CORS_ORIGINS" = concatStringsSep "," cfg.corsOrigins;
        }
        // optionalAttrs cfg.corsAllowAll {"KEPT_CORS_ALLOW_ALL" = "1";}
        // optionalAttrs cfg.allowRestore {"KEPT_ALLOW_RESTORE" = "1";}
        // optionalAttrs (!cfg.linkPreviewScreenshots) {
          "KEPT_LINK_PREVIEW_SCREENSHOTS" = "0";
        }
        // cfg.extraEnv;
      environmentFiles = optional cfg.vapid.enable config.sops.templates."kept-vapid-env".path;
      volumes = [
        "${cfg.baseDir}:/app/data"
      ];
      ports = ["${toString cfg.webPort}:${toString cfg.webPort}"];
      extraOptions =
        ["--network-alias=kept"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "kept";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-kept" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      sopsTemplates = optional cfg.vapid.enable "kept-vapid-env";
    };
  };
}
