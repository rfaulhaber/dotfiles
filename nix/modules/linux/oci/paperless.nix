{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.paperless;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  networkName = "paperless";

  boolEnv = b:
    if b
    then "true"
    else "false";
in {
  options.modules.linux.oci.services.paperless = {
    enable = mkEnableOption "Paperless-ngx document management";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/paperless-ngx/paperless-ngx";
      version = "latest";
    };

    baseDir = mkOption {
      description = ''
        Parent directory for paperless state. Four child datasets:
          <baseDir>/data   → /usr/src/paperless/data (search index, classifier models)
          <baseDir>/media  → /usr/src/paperless/media (the documents). The
                             consume/ and export/ subdirs of this same dataset
                             are bind-mounted onto /usr/src/paperless/consume
                             and /usr/src/paperless/export so the watch folder
                             and export staging inherit media's encryption
                             without needing additional keys.
          <baseDir>/db     → /var/lib/postgresql/data on the postgres sidecar
          <baseDir>/redis  → /data on the broker (redis) sidecar
      '';
      type = types.str;
      example = "/data/apps/paperless";
    };

    port = mkOption {
      description = "Host port for the paperless web UI (proxied to container port 8000).";
      type = types.port;
      default = 8377;
    };

    url = mkOption {
      description = ''
        Public URL where paperless is reachable, no trailing slash. Sets
        PAPERLESS_URL — paperless derives ALLOWED_HOSTS and CSRF_TRUSTED_ORIGINS
        from it, so changing it later requires a container restart.
      '';
      type = types.str;
      example = "https://paperless.3679.space";
    };

    timezone = mkOption {
      description = "PAPERLESS_TIME_ZONE for the paperless container.";
      type = types.str;
      default = "America/New_York";
    };

    ocrLanguage = mkOption {
      description = ''
        Primary OCR language (PAPERLESS_OCR_LANGUAGE) — 3-letter ISO code.
        English, German, Italian, Spanish, French are preinstalled. Add more
        via extraEnv.PAPERLESS_OCR_LANGUAGES.
      '';
      type = types.str;
      default = "eng";
    };

    adminUsername = mkOption {
      description = ''
        Initial superuser username (PAPERLESS_ADMIN_USER). Only consumed when
        no superuser exists yet; subsequent edits go through paperless's UI.
      '';
      type = types.str;
      default = "ryan";
    };

    adminEmail = mkOption {
      description = "PAPERLESS_ADMIN_MAIL for the initial superuser.";
      type = types.str;
      default = "root@localhost";
    };

    user = {
      uid = mkOption {
        description = "USERMAP_UID — UID paperless runs as inside the container.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "USERMAP_GID — GID paperless runs as inside the container.";
        type = types.int;
        default = 100;
      };
    };

    dependsOn = mkOption {
      description = "Other oci-containers the paperless container depends on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Additional env vars passed to the paperless container.";
      type = types.attrsOf types.str;
      default = {};
    };

    postgres = {
      image = imageLib.mkImageOptions {
        repository = "postgres";
        version = "18-alpine";
      };

      user = mkOption {
        description = "PostgreSQL username.";
        type = types.str;
        default = "paperless";
      };

      database = mkOption {
        description = "PostgreSQL database name.";
        type = types.str;
        default = "paperless";
      };

      pgdata = mkOption {
        description = ''
          PGDATA path inside the postgres container. Default matches the
          official image's standard layout.
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

    oidc = {
      enable = mkEnableOption ''
        OpenID Connect SSO via django-allauth's openid_connect provider. When
        true the module requires sops secrets at "paperless/oidc-client-id"
        and "paperless/oidc-client-secret", and renders
        PAPERLESS_SOCIALACCOUNT_PROVIDERS as a JSON env var
      '';

      providerId = mkOption {
        description = ''
          django-allauth provider_id. Becomes part of the OAuth redirect URI:
          ''${url}/accounts/oidc/<providerId>/login/callback/. Keep stable —
          renaming invalidates any IdP registration tied to the previous path.
        '';
        type = types.str;
        default = "pocketid";
      };

      providerName = mkOption {
        description = "Display name shown on the SSO sign-in button.";
        type = types.str;
        default = "Pocket ID";
      };

      serverUrl = mkOption {
        description = ''
          OIDC discovery URL. PocketID serves OpenID configuration at
          /.well-known/openid-configuration off its issuer root, so this is
          typically "''${issuer}/.well-known/openid-configuration".
        '';
        type = types.str;
        example = "https://auth.3679.space/.well-known/openid-configuration";
      };

      disableRegularLogin = mkOption {
        description = ''
          Hide the local username/password form on the login page. The Django
          admin (/admin/) and API still accept local creds as an emergency
          override.
        '';
        type = types.bool;
        default = true;
      };

      redirectLoginToSso = mkOption {
        description = ''
          When true the login page redirects to the first SSO provider via
          JavaScript on load. Disable for IdP-down resilience.
        '';
        type = types.bool;
        default = true;
      };

      defaultGroups = mkOption {
        description = ''
          PAPERLESS_SOCIAL_ACCOUNT_DEFAULT_GROUPS — groups newly-provisioned
          SSO users are auto-added to. Must pre-exist in paperless's UI.
        '';
        type = types.listOf types.str;
        default = [];
      };
    };

    dataEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the data dataset (search index, classifier models — derived
          but sensitive content, since the index contains every OCR'd word)
          using a sops-managed raw key. The dataset is marked canmount=noauto
          and unlocked by a dedicated systemd unit before podman-paperless
          starts.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key for the data dataset.
        '';
        type = types.path;
      };
    };

    mediaEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the media dataset (the documents themselves, plus consume/
          and export/ subdirs that are bind-mounted into the container as
          /usr/src/paperless/consume and /usr/src/paperless/export) using a
          sops-managed raw key. Same wiring as dataEncryption.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key for the media dataset.
        '';
        type = types.path;
      };
    };

    dbEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the db dataset (postgres) using a sops-managed raw key. The
          paperless database stores OCR-extracted text, filenames, and tags,
          so this is the recommended companion to media encryption.
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

    dataProperties = mkOption {
      description = "ZFS properties applied to the data dataset.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    mediaProperties = mkOption {
      description = ''
        ZFS properties applied to the media dataset. Default tuned for
        PDFs/scans — larger blocks compress better and reduce metadata
        overhead for typically multi-MB document files.
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "1M";};
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
    dataDir = "${cfg.baseDir}/data";
    mediaDir = "${cfg.baseDir}/media";
    dbDir = "${cfg.baseDir}/db";
    redisDir = "${cfg.baseDir}/redis";
    consumeDir = "${mediaDir}/consume";
    exportDir = "${mediaDir}/export";

    # Dataset names = mountpoints with leading slash stripped, matching
    # the convention enforced by modules.linux.oci._managedPaths.
    dataDataset = removePrefix "/" dataDir;
    mediaDataset = removePrefix "/" mediaDir;
    dbDataset = removePrefix "/" dbDir;

    dataKeyPath =
      config.sops.secrets."paperless/data-zfs-key".path
      or "/run/secrets/paperless-data-zfs-key";
    mediaKeyPath =
      config.sops.secrets."paperless/media-zfs-key".path
      or "/run/secrets/paperless-media-zfs-key";
    dbKeyPath =
      config.sops.secrets."paperless/db-zfs-key".path
      or "/run/secrets/paperless-db-zfs-key";

    mkEncryptionProps = keyPath: {
      encryption = "aes-256-gcm";
      keyformat = "raw";
      keylocation = "file://${keyPath}";
      canmount = "noauto";
    };

    dataEncryptionProps =
      optionalAttrs cfg.dataEncryption.enable (mkEncryptionProps dataKeyPath);
    mediaEncryptionProps =
      optionalAttrs cfg.mediaEncryption.enable (mkEncryptionProps mediaKeyPath);
    dbEncryptionProps =
      optionalAttrs cfg.dbEncryption.enable (mkEncryptionProps dbKeyPath);
  in {
    modules.linux.oci._managedPaths = {
      # Parent has no mountpoint — only its children are mounted.
      "${cfg.baseDir}".properties.mountpoint = "none";
      # Encryption properties go first so user-supplied *Properties win on conflict.
      ${dataDir}.properties = dataEncryptionProps // cfg.dataProperties;
      ${mediaDir}.properties = mediaEncryptionProps // cfg.mediaProperties;
      ${dbDir}.properties = dbEncryptionProps // cfg.dbProperties;
      ${redisDir}.properties = cfg.redisProperties;
    };

    # Each encrypted dataset gets its own unlock unit. `consumers` makes the
    # consuming podman service depend on the unlock without needing manual
    # After/Requires on the consumer side.
    modules.services.zfs.encryptedDatasets =
      optionalAttrs cfg.dataEncryption.enable {
        paperless-data = {
          dataset = dataDataset;
          keyFile = dataKeyPath;
          consumers = ["podman-paperless.service"];
        };
      }
      // optionalAttrs cfg.mediaEncryption.enable {
        paperless-media = {
          dataset = mediaDataset;
          keyFile = mediaKeyPath;
          consumers = ["podman-paperless.service"];
        };
      }
      // optionalAttrs cfg.dbEncryption.enable {
        paperless-db = {
          dataset = dbDataset;
          keyFile = dbKeyPath;
          consumers = ["podman-paperless_db.service"];
        };
      };

    # Dedicated network so postgres + redis stay isolated from other apps.
    modules.linux.oci.networks.${networkName}.enable = true;

    sops.secrets =
      {
        "paperless/secret-key" = {};
        "paperless/admin-password" = {};
        "paperless/db-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "paperless/oidc-client-id" = {};
        "paperless/oidc-client-secret" = {};
      }
      // optionalAttrs cfg.dataEncryption.enable {
        "paperless/data-zfs-key" = {
          format = "binary";
          sopsFile = cfg.dataEncryption.keyFile;
        };
      }
      // optionalAttrs cfg.mediaEncryption.enable {
        "paperless/media-zfs-key" = {
          format = "binary";
          sopsFile = cfg.mediaEncryption.keyFile;
        };
      }
      // optionalAttrs cfg.dbEncryption.enable {
        "paperless/db-zfs-key" = {
          format = "binary";
          sopsFile = cfg.dbEncryption.keyFile;
        };
      };

    sops.templates =
      {
        # Shared by paperless (PAPERLESS_DBPASS read at runtime by Django) and
        # paperless_db (POSTGRES_PASSWORD consumed on initdb only).
        "paperless-db-env".content = ''
          POSTGRES_PASSWORD=${config.sops.placeholder."paperless/db-password"}
          PAPERLESS_DBPASS=${config.sops.placeholder."paperless/db-password"}
        '';

        # SECRET_KEY signs sessions/CSRF tokens; rotating it invalidates all
        # active sessions. ADMIN_PASSWORD is only consumed when no superuser
        # exists yet — safe to leave set across the container's lifetime.
        "paperless-app-env".content = ''
          PAPERLESS_SECRET_KEY=${config.sops.placeholder."paperless/secret-key"}
          PAPERLESS_ADMIN_PASSWORD=${config.sops.placeholder."paperless/admin-password"}
        '';
      }
      // optionalAttrs cfg.oidc.enable {
        # PAPERLESS_SOCIALACCOUNT_PROVIDERS is a single-line JSON env var
        # that django-allauth parses on boot. Embedded client_id and secret
        # are sops placeholders, substituted at activation time.
        "paperless-oidc-env".content = let
          providersJson = builtins.toJSON {
            openid_connect.APPS = [
              {
                provider_id = cfg.oidc.providerId;
                name = cfg.oidc.providerName;
                client_id = config.sops.placeholder."paperless/oidc-client-id";
                secret = config.sops.placeholder."paperless/oidc-client-secret";
                settings.server_url = cfg.oidc.serverUrl;
              }
            ];
          };
        in ''
          PAPERLESS_SOCIALACCOUNT_PROVIDERS=${providersJson}
        '';
      };

    virtualisation.oci-containers.containers = let
      appEnv =
        {
          "USERMAP_UID" = toString cfg.user.uid;
          "USERMAP_GID" = toString cfg.user.gid;
          "PAPERLESS_REDIS" = "redis://paperless_broker:6379";
          "PAPERLESS_DBENGINE" = "postgresql";
          "PAPERLESS_DBHOST" = "paperless_db";
          "PAPERLESS_DBPORT" = "5432";
          "PAPERLESS_DBNAME" = cfg.postgres.database;
          "PAPERLESS_DBUSER" = cfg.postgres.user;
          "PAPERLESS_URL" = cfg.url;
          "PAPERLESS_TIME_ZONE" = cfg.timezone;
          "PAPERLESS_OCR_LANGUAGE" = cfg.ocrLanguage;
          "PAPERLESS_ADMIN_USER" = cfg.adminUsername;
          "PAPERLESS_ADMIN_MAIL" = cfg.adminEmail;
        }
        // optionalAttrs cfg.oidc.enable {
          "PAPERLESS_APPS" = "allauth.socialaccount.providers.openid_connect";
          "PAPERLESS_DISABLE_REGULAR_LOGIN" = boolEnv cfg.oidc.disableRegularLogin;
          "PAPERLESS_REDIRECT_LOGIN_TO_SSO" = boolEnv cfg.oidc.redirectLoginToSso;
        }
        // optionalAttrs (cfg.oidc.enable && cfg.oidc.defaultGroups != []) {
          "PAPERLESS_SOCIAL_ACCOUNT_DEFAULT_GROUPS" =
            concatStringsSep "," cfg.oidc.defaultGroups;
        }
        // cfg.extraEnv;

      appEnvFiles =
        [
          config.sops.templates."paperless-db-env".path
          config.sops.templates."paperless-app-env".path
        ]
        ++ optional cfg.oidc.enable config.sops.templates."paperless-oidc-env".path;
    in {
      "paperless_db" = {
        image = imageLib.renderImage cfg.postgres.image;
        environment = {
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
          "PGDATA" = cfg.postgres.pgdata;
          "TZ" = cfg.timezone;
        };
        environmentFiles = [config.sops.templates."paperless-db-env".path];
        volumes = [
          "${dbDir}:/var/lib/postgresql/data"
        ];
        extraOptions =
          [
            "--network-alias=paperless_db"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "paperless.postgres";
            image = cfg.postgres.image;
          };
        log-driver = "journald";
      };

      "paperless_broker" = {
        image = imageLib.renderImage cfg.redis.image;
        environment = {"TZ" = cfg.timezone;};
        volumes = [
          "${redisDir}:/data"
        ];
        extraOptions =
          [
            "--network-alias=paperless_broker"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "paperless.broker";
            image = cfg.redis.image;
          };
        log-driver = "journald";
      };

      "paperless" = {
        image = imageLib.renderImage cfg.image;
        dependsOn = ["paperless_db" "paperless_broker"] ++ cfg.dependsOn;
        environment = appEnv;
        environmentFiles = appEnvFiles;
        volumes = [
          "${dataDir}:/usr/src/paperless/data"
          "${mediaDir}:/usr/src/paperless/media"
          # consume + export ride inside the media dataset — they inherit
          # media's ZFS encryption without a separate key.
          "${consumeDir}:/usr/src/paperless/consume"
          "${exportDir}:/usr/src/paperless/export"
        ];
        ports = [
          "${toString cfg.port}:8000"
        ];
        extraOptions =
          [
            "--network-alias=paperless"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "paperless";
            image = cfg.image;
          };
        log-driver = "journald";
      };
    };

    systemd.services = {
      "podman-paperless_db" = ociLib.mkServiceConfig {
        networks = [networkName];
        sopsTemplates = ["paperless-db-env"];
      };

      "podman-paperless_broker" = ociLib.mkServiceConfig {
        networks = [networkName];
      };

      "podman-paperless" = mkMerge [
        (ociLib.mkServiceConfig {
          networks = [networkName];
          extraAfter = ["podman-paperless_db.service" "podman-paperless_broker.service"];
          extraRequires = ["podman-paperless_db.service" "podman-paperless_broker.service"];
          sopsTemplates =
            ["paperless-db-env" "paperless-app-env"]
            ++ optional cfg.oidc.enable "paperless-oidc-env";
        })
        {
          # Create the in-dataset consume/ and export/ subdirectories before
          # paperless starts. The media dataset is already mounted at this
          # point — modules.services.zfs.encryptedDatasets.paperless-media.consumers
          # above orders the unlock unit ahead of podman-paperless.service.
          serviceConfig.ExecStartPre = [
            "${pkgs.coreutils}/bin/mkdir -p ${consumeDir} ${exportDir}"
          ];
        }
      ];
    };
  });
}
