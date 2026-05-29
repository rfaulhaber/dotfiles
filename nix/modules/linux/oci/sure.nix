{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.sure;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  networkName = "sure";

  boolEnv = b:
    if b
    then "true"
    else "false";
in {
  options.modules.linux.oci.services.sure = {
    enable = mkEnableOption "Sure personal finance app";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/we-promise/sure";
      version = "stable";
    };

    baseDir = mkOption {
      description = ''
        Parent directory for Sure state. Three children are bind-mounted into
        containers: <baseDir>/storage (Rails active-storage uploads, mounted at
        /rails/storage on the web + worker containers), <baseDir>/db (postgres,
        mounted at /var/lib/postgresql/data on sure_db), and <baseDir>/redis
        (mounted at /data on sure_redis).
      '';
      type = types.str;
      example = "/data/apps/sure";
    };

    port = mkOption {
      description = "Host port for the Sure web UI (proxied to container port 3000).";
      type = types.port;
      default = 4274;
    };

    appUrl = mkOption {
      description = ''
        Public URL where Sure is reachable. Used as APP_URL inside the
        container (for SAML ACS URLs, password-reset links, etc.) and as the
        default base for oidc.redirectUri.
      '';
      type = types.str;
      example = "https://sure.3679.space";
    };

    assumeSsl = mkOption {
      description = ''
        Set RAILS_ASSUME_SSL=true so Rails treats inbound HTTP requests as
        having been terminated by an upstream proxy. Required when the
        public appUrl is https:// and a reverse proxy (Pangolin/Caddy)
        forwards plaintext to the container.
      '';
      type = types.bool;
      default = true;
    };

    forceSsl = mkOption {
      description = ''
        Set RAILS_FORCE_SSL=true to make Rails redirect insecure requests.
        Leave false when an upstream proxy already enforces HTTPS — enabling
        both can cause redirect loops.
      '';
      type = types.bool;
      default = false;
    };

    selfHosted = mkOption {
      description = "Toggle Sure's self-hosted feature set (SELF_HOSTED env).";
      type = types.bool;
      default = true;
    };

    onboardingState = mkOption {
      description = ''
        Controls how new users register. "open" allows anyone to sign up;
        "closed" disables registration entirely; "invite_only" requires an
        invitation. Start with "open" to register the first super-admin,
        then flip to "closed" or "invite_only".
      '';
      type = types.enum ["open" "closed" "invite_only"];
      default = "open";
    };

    localLogin = {
      enabled = mkOption {
        description = ''
          When true, the login page shows the email/password form. When false,
          users must sign in via SSO unless adminOverrideEnabled is true.
        '';
        type = types.bool;
        default = true;
      };

      adminOverrideEnabled = mkOption {
        description = ''
          When true and localLogin.enabled is false, super-admin users can
          still log in with local passwords (emergency override during IdP
          outages).
        '';
        type = types.bool;
        default = false;
      };
    };

    jit = {
      mode = mkOption {
        description = ''
          JIT user-provisioning mode. "create_and_link" creates new users
          from verified SSO identities; "link_only" requires an existing
          account before SSO linking is allowed.
        '';
        type = types.enum ["create_and_link" "link_only"];
        default = "create_and_link";
      };

      allowedDomains = mkOption {
        description = ''
          Optional comma-separated list of email domains permitted to JIT
          create accounts via SSO. Empty string allows all domains.
        '';
        type = types.str;
        default = "";
        example = "3679.space";
      };
    };

    oidc = {
      enable = mkEnableOption ''
        OpenID Connect SSO. When true the module requires sops secrets at
        "sure/oidc-client-id" and "sure/oidc-client-secret"
      '';

      issuer = mkOption {
        description = ''
          OIDC issuer URL. Sure uses OpenID-Connect discovery, so this must
          serve /.well-known/openid-configuration (PocketID does at its
          root).
        '';
        type = types.str;
        example = "https://auth.3679.space";
      };

      redirectUri = mkOption {
        description = ''
          OAuth2 redirect URI registered with the IdP. Defaults to
          "''${appUrl}/auth/openid_connect/callback" — override if you've
          renamed the provider in config/auth.yml.
        '';
        type = types.str;
        defaultText = literalExpression "\"\${cfg.appUrl}/auth/openid_connect/callback\"";
        default = "${cfg.appUrl}/auth/openid_connect/callback";
      };

      buttonLabel = mkOption {
        description = "Label shown on the SSO sign-in button.";
        type = types.str;
        default = "Sign in with Pocket ID";
      };

      buttonIcon = mkOption {
        description = "Icon shown on the SSO sign-in button (Sure icon name).";
        type = types.str;
        default = "key";
      };
    };

    postgres = {
      image = imageLib.mkImageOptions {
        # Sure's compose pins postgres:16 — keeping the major lets pg_dumpall
        # round-trips work cleanly. -alpine is the canonical small image.
        repository = "postgres";
        version = "16-alpine";
      };

      user = mkOption {
        description = "PostgreSQL username.";
        type = types.str;
        default = "sure";
      };

      database = mkOption {
        description = "PostgreSQL database name.";
        type = types.str;
        default = "sure_production";
      };

      pgdata = mkOption {
        description = ''
          PGDATA path inside the postgres container. Default matches the
          official image's standard layout. Override only when a legacy
          on-disk layout nests the cluster deeper.
        '';
        type = types.str;
        default = "/var/lib/postgresql/data";
      };
    };

    redis = {
      image = imageLib.mkImageOptions {
        repository = "redis";
        version = "8-alpine";
      };
    };

    storageEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the storage dataset using a sops-managed raw key. Adds
          encryption properties to the dataset (immutable after creation),
          marks it canmount=noauto so the early `zfs-mount.service` skips
          it, and wires a dedicated unlock unit that runs before the
          sure + sure_worker containers start.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key. Treated as `format = "binary"` by sops-nix.
        '';
        type = types.path;
      };
    };

    dbEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the db (postgres) dataset using a sops-managed raw key.
          Same wiring as storageEncryption — postgres waits for the unlock
          unit before starting.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key for the db dataset.
        '';
        type = types.path;
      };
    };

    timezone = mkOption {
      description = "TZ env var passed to all containers.";
      type = types.str;
      default = "America/New_York";
    };

    networks = mkOption {
      description = ''
        Networks the Sure containers join. The dedicated "sure" network
        keeps the database and redis isolated from the default network.
      '';
      type = types.listOf types.str;
      default = [networkName];
    };

    dependsOn = mkOption {
      description = "Other oci-containers the web/worker containers depend on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Additional env vars passed to the web + worker containers.";
      type = types.attrsOf types.str;
      default = {};
    };

    storageProperties = mkOption {
      description = "ZFS properties applied to the storage dataset (Rails uploads).";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    dbProperties = mkOption {
      description = "ZFS properties applied to the db dataset. Defaults tuned for postgres.";
      type = types.attrsOf types.str;
      default = {recordsize = "8K";};
    };

    redisProperties = mkOption {
      description = "ZFS properties applied to the redis dataset.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    storageDir = "${cfg.baseDir}/storage";
    dbDir = "${cfg.baseDir}/db";
    redisDir = "${cfg.baseDir}/redis";
    # Dataset names = mountpoints with leading slash stripped, matching
    # the convention enforced by modules.linux.oci._managedPaths.
    storageDataset = removePrefix "/" storageDir;
    dbDataset = removePrefix "/" dbDir;
    storageKeyPath =
      config.sops.secrets."sure/storage-zfs-key".path
      or "/run/secrets/sure-storage-zfs-key";
    dbKeyPath =
      config.sops.secrets."sure/db-zfs-key".path
      or "/run/secrets/sure-db-zfs-key";
    storageEncryptionProperties = optionalAttrs cfg.storageEncryption.enable {
      encryption = "aes-256-gcm";
      keyformat = "raw";
      keylocation = "file://${storageKeyPath}";
      canmount = "noauto";
    };
    dbEncryptionProperties = optionalAttrs cfg.dbEncryption.enable {
      encryption = "aes-256-gcm";
      keyformat = "raw";
      keylocation = "file://${dbKeyPath}";
      canmount = "noauto";
    };
  in {
    modules.linux.oci._managedPaths = {
      "${cfg.baseDir}".properties.mountpoint = "none";
      # Encryption properties go first so user-supplied *Properties win on conflict.
      ${storageDir}.properties = storageEncryptionProperties // cfg.storageProperties;
      ${dbDir}.properties = dbEncryptionProperties // cfg.dbProperties;
      ${redisDir}.properties = cfg.redisProperties;
    };

    # Wire dedicated unlock units. The `consumers` relationship makes the
    # podman services wait for the unlock without needing manual After/
    # Requires on the container side.
    modules.services.zfs.encryptedDatasets =
      optionalAttrs cfg.storageEncryption.enable {
        sure-storage = {
          dataset = storageDataset;
          keyFile = storageKeyPath;
          consumers = ["podman-sure.service" "podman-sure_worker.service"];
        };
      }
      // optionalAttrs cfg.dbEncryption.enable {
        sure-db = {
          dataset = dbDataset;
          keyFile = dbKeyPath;
          consumers = ["podman-sure_db.service"];
        };
      };

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "sure/secret-key-base" = {};
        "sure/db-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "sure/oidc-client-id" = {};
        "sure/oidc-client-secret" = {};
      }
      // optionalAttrs cfg.storageEncryption.enable {
        # Treated as opaque 32 raw bytes — any text round-trip would corrupt it.
        "sure/storage-zfs-key" = {
          format = "binary";
          sopsFile = cfg.storageEncryption.keyFile;
        };
      }
      // optionalAttrs cfg.dbEncryption.enable {
        "sure/db-zfs-key" = {
          format = "binary";
          sopsFile = cfg.dbEncryption.keyFile;
        };
      };

    sops.templates =
      {
        # Shared by sure_db (POSTGRES_PASSWORD consumed by initdb on first
        # boot) and by the web + worker containers (POSTGRES_PASSWORD read
        # by Rails' config/database.yml at runtime via ENV.fetch).
        "sure-db-env".content = ''
          POSTGRES_PASSWORD=${config.sops.placeholder."sure/db-password"}
        '';

        # Read by the web + worker containers. SECRET_KEY_BASE signs
        # cookies/sessions and derives the ActiveRecord encryption keys, so
        # rotating it invalidates all sessions and breaks any pre-existing
        # encrypted columns.
        "sure-app-env".content = ''
          SECRET_KEY_BASE=${config.sops.placeholder."sure/secret-key-base"}
        '';
      }
      // optionalAttrs cfg.oidc.enable {
        "sure-oidc-env".content = ''
          OIDC_CLIENT_ID=${config.sops.placeholder."sure/oidc-client-id"}
          OIDC_CLIENT_SECRET=${config.sops.placeholder."sure/oidc-client-secret"}
        '';
      };

    virtualisation.oci-containers.containers = let
      appEnv =
        {
          "SELF_HOSTED" = boolEnv cfg.selfHosted;
          "ONBOARDING_STATE" = cfg.onboardingState;
          "RAILS_FORCE_SSL" = boolEnv cfg.forceSsl;
          "RAILS_ASSUME_SSL" = boolEnv cfg.assumeSsl;
          "APP_URL" = cfg.appUrl;
          "DB_HOST" = "sure_db";
          "DB_PORT" = "5432";
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
          "REDIS_URL" = "redis://sure_redis:6379/1";
          "TZ" = cfg.timezone;
          "AUTH_LOCAL_LOGIN_ENABLED" = boolEnv cfg.localLogin.enabled;
          "AUTH_LOCAL_ADMIN_OVERRIDE_ENABLED" = boolEnv cfg.localLogin.adminOverrideEnabled;
          "AUTH_JIT_MODE" = cfg.jit.mode;
          "ALLOWED_OIDC_DOMAINS" = cfg.jit.allowedDomains;
        }
        // optionalAttrs cfg.oidc.enable {
          "OIDC_ISSUER" = cfg.oidc.issuer;
          "OIDC_REDIRECT_URI" = cfg.oidc.redirectUri;
          "OIDC_BUTTON_LABEL" = cfg.oidc.buttonLabel;
          "OIDC_BUTTON_ICON" = cfg.oidc.buttonIcon;
        }
        // cfg.extraEnv;

      appEnvFiles =
        [
          config.sops.templates."sure-db-env".path
          config.sops.templates."sure-app-env".path
        ]
        ++ optional cfg.oidc.enable config.sops.templates."sure-oidc-env".path;
    in {
      "sure_db" = {
        image = imageLib.renderImage cfg.postgres.image;
        environment = {
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
          "PGDATA" = cfg.postgres.pgdata;
          "TZ" = cfg.timezone;
        };
        environmentFiles = [config.sops.templates."sure-db-env".path];
        volumes = [
          "${dbDir}:/var/lib/postgresql/data"
        ];
        extraOptions =
          [
            "--network-alias=sure_db"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "sure.postgres";
            image = cfg.postgres.image;
          };
        log-driver = "journald";
      };

      "sure_redis" = {
        image = imageLib.renderImage cfg.redis.image;
        environment = {
          "TZ" = cfg.timezone;
        };
        # AOF + 60s/1000-change RDB gives Rails' Sidekiq job state a low
        # data-loss window without the write amplification of full AOF.
        cmd = ["redis-server" "--appendonly" "yes" "--save" "60" "1000"];
        volumes = [
          "${redisDir}:/data"
        ];
        extraOptions =
          [
            "--network-alias=sure_redis"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "sure.redis";
            image = cfg.redis.image;
          };
        log-driver = "journald";
      };

      "sure" = {
        image = imageLib.renderImage cfg.image;
        dependsOn = ["sure_db" "sure_redis"] ++ cfg.dependsOn;
        environment = appEnv;
        environmentFiles = appEnvFiles;
        volumes = [
          "${storageDir}:/rails/storage"
        ];
        ports = [
          "${toString cfg.port}:3000"
        ];
        extraOptions =
          [
            "--network-alias=sure"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "sure";
            image = cfg.image;
          };
        log-driver = "journald";
      };

      "sure_worker" = {
        image = imageLib.renderImage cfg.image;
        dependsOn = ["sure_db" "sure_redis"] ++ cfg.dependsOn;
        environment = appEnv;
        environmentFiles = appEnvFiles;
        # Sidekiq sidecar — same Rails image, different process entry.
        # No host ports; talks to postgres + redis over the sure network.
        cmd = ["bundle" "exec" "sidekiq"];
        volumes = [
          "${storageDir}:/rails/storage"
        ];
        extraOptions =
          [
            "--network-alias=sure_worker"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "sure.worker";
            image = cfg.image;
          };
        log-driver = "journald";
      };
    };

    systemd.services = {
      "podman-sure_db" = ociLib.mkServiceConfig {
        networks = [networkName];
        sopsTemplates = ["sure-db-env"];
      };

      "podman-sure_redis" = ociLib.mkServiceConfig {
        networks = [networkName];
      };

      "podman-sure" = ociLib.mkServiceConfig {
        networks = [networkName];
        extraAfter = ["podman-sure_db.service" "podman-sure_redis.service"];
        extraRequires = ["podman-sure_db.service" "podman-sure_redis.service"];
        sopsTemplates =
          ["sure-db-env" "sure-app-env"]
          ++ optional cfg.oidc.enable "sure-oidc-env";
      };

      "podman-sure_worker" = ociLib.mkServiceConfig {
        networks = [networkName];
        extraAfter = ["podman-sure_db.service" "podman-sure_redis.service"];
        extraRequires = ["podman-sure_db.service" "podman-sure_redis.service"];
        sopsTemplates =
          ["sure-db-env" "sure-app-env"]
          ++ optional cfg.oidc.enable "sure-oidc-env";
      };
    };
  });
}
