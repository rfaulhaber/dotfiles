{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.forgejo;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  networkName = "forgejo";
in {
  options.modules.linux.oci.services.forgejo = {
    enable = mkEnableOption "Forgejo git server";

    image = imageLib.mkImageOptions {
      repository = "codeberg.org/forgejo/forgejo";
      version = "15.0.0";
    };

    baseDir = mkOption {
      description = ''
        Parent directory for forgejo state. Two children are bind-mounted:
        <baseDir>/data → /data (forgejo's repos, attachments, sessions),
        <baseDir>/db → /var/lib/postgresql/data.
      '';
      type = types.str;
      example = "/data/apps/forgejo";
    };

    domain = mkOption {
      description = "FORGEJO__server__DOMAIN — public hostname.";
      type = types.str;
      example = "git.example.com";
    };

    rootUrl = mkOption {
      description = "FORGEJO__server__ROOT_URL — full external URL.";
      type = types.str;
      example = "https://git.example.com";
    };

    webPort = mkOption {
      description = "Host port for the forgejo web UI.";
      type = types.port;
      default = 2835;
    };

    sshPort = mkOption {
      description = "Host port for forgejo SSH.";
      type = types.port;
      default = 3402;
    };

    sshDomain = mkOption {
      description = "FORGEJO__server__SSH_DOMAIN — hostname used in clone URLs.";
      type = types.str;
      example = "git.example.com";
    };

    advertisedSshPort = mkOption {
      description = ''
        FORGEJO__server__SSH_PORT — port advertised in clone URLs. Usually
        22 when an external load balancer or DNAT rule forwards 22 to
        sshPort; otherwise set to sshPort.
      '';
      type = types.port;
      default = 22;
    };

    enableReverseProxyAuth = mkOption {
      description = ''
        FORGEJO__service__ENABLE_REVERSE_PROXY_AUTHENTICATION. Authenticates
        a user from a request header, so it is only as strong as
        trustedProxies — any peer trusted there can assume any account,
        including an admin one.
      '';
      type = types.bool;
      default = false;
    };

    trustedProxies = mkOption {
      description = ''
        FORGEJO__security__REVERSE_PROXY_TRUSTED_PROXIES — peers whose
        X-Forwarded-For (and, when enableReverseProxyAuth is set, whose
        auth header) forgejo honors.

        Emitted unconditionally. The container image seeds app.ini with a
        permissive `*` at install time and never revisits that file on
        upgrade, so on an existing deployment an explicit env override is
        the only thing that clears it.
      '';
      type = types.listOf types.str;
      default = ["127.0.0.1/8" "::1/128"];
      example = ["10.89.0.0/16" "192.168.0.2/32"];
    };

    user = {
      uid = mkOption {
        description = "USER_UID inside the container.";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "USER_GID inside the container.";
        type = types.int;
        default = 100;
      };
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on (in addition to forgejo_db).";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Additional FORGEJO__* environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    postgres = {
      image = imageLib.mkImageOptions {
        repository = "postgres";
        version = "17.2-alpine";
      };

      user = mkOption {
        description = "PostgreSQL username (and forgejo DB user).";
        type = types.str;
        default = "forgejo";
      };

      database = mkOption {
        description = "PostgreSQL database name.";
        type = types.str;
        default = "forgejo";
      };

      port = mkOption {
        description = ''
          Host port to expose postgres on. The compose file published 8256
          for direct DB access; set to null to keep postgres internal-only.
        '';
        type = types.nullOr types.port;
        default = null;
      };
    };

    dataProperties = mkOption {
      description = "ZFS properties applied to the data dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    dbProperties = mkOption {
      description = "ZFS properties applied to the db dataset. Defaults tuned for postgres.";
      type = types.attrsOf types.str;
      default = {recordsize = "8K";};
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.enableReverseProxyAuth -> cfg.trustedProxies != [];
        message = ''
          modules.linux.oci.services.forgejo.enableReverseProxyAuth is enabled
          with an empty trustedProxies list. Forgejo would honor the
          reverse-proxy auth header from every peer, letting anyone that can
          reach it log in as any account.
        '';
      }
    ];

    modules.linux.oci._managedPaths = {
      # Parent dataset has no mountpoint — only its children are mounted.
      "${cfg.baseDir}".properties.mountpoint = "none";
      "${cfg.baseDir}/data".properties = cfg.dataProperties;
      "${cfg.baseDir}/db".properties = cfg.dbProperties;
    };

    modules.linux.oci.networks.${networkName}.enable = true;

    sops.secrets."forgejo/db-password" = {};

    # Shared between forgejo (FORGEJO__database__PASSWD) and the postgres
    # sidecar (POSTGRES_PASSWORD). Both consume the same env file.
    sops.templates."forgejo-db-env".content = ''
      FORGEJO__database__PASSWD=${config.sops.placeholder."forgejo/db-password"}
      POSTGRES_PASSWORD=${config.sops.placeholder."forgejo/db-password"}
    '';

    virtualisation.oci-containers.containers = {
      "forgejo_db" = {
        image = imageLib.renderImage cfg.postgres.image;
        environment = {
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
        };
        environmentFiles = [config.sops.templates."forgejo-db-env".path];
        volumes = [
          "${cfg.baseDir}/db:/var/lib/postgresql/data"
        ];
        ports = optional (cfg.postgres.port != null) "${toString cfg.postgres.port}:5432";
        extraOptions =
          [
            "--network-alias=forgejo_db"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "forgejo.postgres";
            image = cfg.postgres.image;
          };
        log-driver = "journald";
      };

      "forgejo" = {
        image = imageLib.renderImage cfg.image;
        dependsOn = ["forgejo_db"] ++ cfg.dependsOn;
        environment =
          {
            "USER_UID" = toString cfg.user.uid;
            "USER_GID" = toString cfg.user.gid;
            "TZ" = cfg.timezone;
            "FORGEJO__database__DB_TYPE" = "postgres";
            "FORGEJO__database__HOST" = "forgejo_db:5432";
            "FORGEJO__database__NAME" = cfg.postgres.database;
            "FORGEJO__database__USER" = cfg.postgres.user;
            "FORGEJO__service__ENABLE_REVERSE_PROXY_AUTHENTICATION" =
              if cfg.enableReverseProxyAuth
              then "true"
              else "false";
            "FORGEJO__security__REVERSE_PROXY_TRUSTED_PROXIES" =
              concatStringsSep "," cfg.trustedProxies;
            "FORGEJO__server__DOMAIN" = cfg.domain;
            "FORGEJO__server__ROOT_URL" = cfg.rootUrl;
            "FORGEJO__server__SSH_DOMAIN" = cfg.sshDomain;
            "FORGEJO__server__SSH_PORT" = toString cfg.advertisedSshPort;
            "FORGEJO__server__SSH_LISTEN_PORT" = "22";
          }
          // cfg.extraEnv;
        environmentFiles = [config.sops.templates."forgejo-db-env".path];
        volumes = [
          "${cfg.baseDir}/data:/data"
          "/etc/timezone:/etc/timezone:ro"
          "/etc/localtime:/etc/localtime:ro"
        ];
        ports = [
          "${toString cfg.webPort}:3000"
          "${toString cfg.sshPort}:22"
        ];
        extraOptions =
          [
            "--network-alias=forgejo"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "forgejo";
            image = cfg.image;
          };
        log-driver = "journald";
      };
    };

    systemd.services = {
      "podman-forgejo_db" = ociLib.mkServiceConfig {
        networks = [networkName];
        sopsTemplates = ["forgejo-db-env"];
      };

      "podman-forgejo" = ociLib.mkServiceConfig {
        networks = [networkName];
        extraAfter = ["podman-forgejo_db.service"];
        extraRequires = ["podman-forgejo_db.service"];
        sopsTemplates = ["forgejo-db-env"];
      };
    };
  };
}
