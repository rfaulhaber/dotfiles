{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.koito;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  boolEnv = b:
    if b
    then "true"
    else "false";
in {
  options.modules.linux.oci.services.koito = {
    enable = mkEnableOption "Koito music listening history tracker";

    image = imageLib.mkImageOptions {
      # Docker Hub — upstream publishes no ghcr images. The release
      # workflow sets `flavor: prefix=v`, so tags carry the v (v0.3.2,
      # not 0.3.2). Pinned rather than floating because this is a 0.x
      # project that has already shipped a release refusing to start on
      # its predecessor's config: v0.2.1 turned KOITO_DATABASE_URL into a
      # fatal error when it dropped PostgreSQL for SQLite.
      repository = "gabehf/koito";
      version = "v0.3.2";
    };

    baseDir = mkOption {
      description = ''
        Single state directory, mounted at Koito's default KOITO_CONFIG_DIR
        of /etc/koito. Holds koito.db (SQLite in WAL mode — the only
        supported backend since v0.2.1), the cover-art cache, and the
        import/ staging directory.
      '';
      type = types.str;
      example = "/data/apps/koito";
    };

    port = mkOption {
      description = ''
        Host port for the web UI and the ListenBrainz-compatible ingest
        API. The container keeps upstream's default of 4110 internally.
      '';
      type = types.port;
      default = 4110;
    };

    adminUsername = mkOption {
      description = ''
        Username of the admin account created on first boot, and only
        while the users table is empty — later starts ignore it. The
        password comes from the sops secret "koito/admin-password"; both
        matter, because unset values silently fall back to admin/changeme.
      '';
      type = types.str;
      default = "admin";
    };

    loginGate = mkOption {
      description = ''
        Require a session before any listening statistics render. Defaults
        to true: the natural deployment is behind a public tunnel, where
        the alternative serves the complete listen history to anyone
        holding the URL.
      '';
      type = types.bool;
      default = true;
    };

    forceTimezone = mkOption {
      description = ''
        Pin the timezone that listens are bucketed into for day/week/month
        statistics. This is not the container clock — see `timezone`. It
        overrides the `?tz=` parameter the web UI derives from each
        viewer's browser, so setting it makes every client see the
        server's day boundaries instead of their own. Leave null to keep
        per-viewer bucketing. An invalid IANA name is fatal at startup.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "America/New_York";
    };

    subsonic = {
      enable = mkEnableOption ''
        cover art sourced from a Subsonic-compatible server rather than
        Deezer and the Cover Art Archive. When true the module requires a
        sops secret at "koito/subsonic-params"
      '';

      url = mkOption {
        description = ''
          Base URL of the Subsonic server as resolved from inside the
          container — a podman network alias, not a host address.
        '';
        type = types.str;
        example = "http://navidrome:4533";
      };
    };

    relay = {
      enable = mkEnableOption ''
        relaying every accepted listen onward to an upstream ListenBrainz
        server. When true the module requires a sops secret at
        "koito/relay-token"
      '';

      url = mkOption {
        description = "Upstream ListenBrainz-compatible API root to relay submissions to.";
        type = types.str;
        default = "https://api.listenbrainz.org";
      };
    };

    timezone = mkOption {
      description = ''
        TZ env var for the container clock. Statistics bucketing is a
        separate knob — see `forceTimezone`.
      '';
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
      description = "Additional KOITO_* environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = ''
        ZFS properties applied to the baseDir dataset. Defaults tuned for
        SQLite alongside the cover-art cache sharing the same directory.
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "koito/admin-password" = {};
      }
      // optionalAttrs cfg.subsonic.enable {
        "koito/subsonic-params" = {};
      }
      // optionalAttrs cfg.relay.enable {
        "koito/relay-token" = {};
      };

    sops.templates."koito-env".content =
      ''
        KOITO_DEFAULT_PASSWORD=${config.sops.placeholder."koito/admin-password"}
      ''
      # The whole Subsonic query string, not just a password: it carries the
      # username and the salted token (u=&t=&s=&v=&c=), all of which identify
      # the account.
      + optionalString cfg.subsonic.enable ''
        KOITO_SUBSONIC_PARAMS=${config.sops.placeholder."koito/subsonic-params"}
      ''
      + optionalString cfg.relay.enable ''
        KOITO_LBZ_RELAY_TOKEN=${config.sops.placeholder."koito/relay-token"}
      '';

    virtualisation.oci-containers.containers.koito = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "TZ" = cfg.timezone;
          "KOITO_DEFAULT_USERNAME" = cfg.adminUsername;
          "KOITO_LOGIN_GATE" = boolEnv cfg.loginGate;
        }
        // optionalAttrs (cfg.forceTimezone != null) {
          "KOITO_FORCE_TZ" = cfg.forceTimezone;
        }
        // optionalAttrs cfg.subsonic.enable {
          "KOITO_SUBSONIC_URL" = cfg.subsonic.url;
        }
        // optionalAttrs cfg.relay.enable {
          "KOITO_ENABLE_LBZ_RELAY" = "true";
          "KOITO_LBZ_RELAY_URL" = cfg.relay.url;
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."koito-env".path];
      volumes = [
        "${cfg.baseDir}:/etc/koito"
      ];
      ports = [
        "${toString cfg.port}:4110"
      ];
      extraOptions =
        [
          "--network-alias=koito"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "koito";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-koito" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      sopsTemplates = ["koito-env"];
    };
  };
}
