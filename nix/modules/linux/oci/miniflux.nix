{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.miniflux;
  ociLib = config.modules.linux.oci.lib;
  networkName = "miniflux";
in {
  options.modules.linux.oci.services.miniflux = {
    enable = mkEnableOption "Miniflux RSS reader";

    baseDir = mkOption {
      description = "Base directory for Miniflux database storage.";
      type = types.str;
      example = "/data/db/miniflux";
    };

    port = mkOption {
      description = "Host port for the web interface.";
      type = types.port;
      default = 4640;
    };

    # Secrets - all should be sops secret paths
    secrets = {
      databasePasswordFile = mkOption {
        description = "Path to file containing postgres password (sops secret).";
        type = types.path;
        example = literalExpression "config.sops.secrets.miniflux-db-password.path";
      };

      adminPasswordFile = mkOption {
        description = "Path to file containing admin password (sops secret).";
        type = types.path;
        example = literalExpression "config.sops.secrets.miniflux-admin-password.path";
      };

      # Optional OIDC configuration
      oidc = {
        enable = mkEnableOption "OIDC authentication";

        clientIdFile = mkOption {
          description = "Path to file containing OIDC client ID (sops secret).";
          type = types.nullOr types.path;
          default = null;
        };

        clientSecretFile = mkOption {
          description = "Path to file containing OIDC client secret (sops secret).";
          type = types.nullOr types.path;
          default = null;
        };

        discoveryEndpoint = mkOption {
          description = "OIDC discovery endpoint URL (no trailing slash).";
          type = types.str;
          example = "https://auth.example.com";
        };

        redirectUrl = mkOption {
          description = "OAuth2 redirect URL.";
          type = types.str;
          example = "https://rss.example.com/oauth2/oidc/callback";
        };

        providerName = mkOption {
          description = "Display name for the OIDC provider.";
          type = types.str;
          default = "SSO";
        };

        userCreation = mkOption {
          description = "Automatically create users from OIDC.";
          type = types.bool;
          default = false;
        };
      };
    };

    postgres = {
      image = mkOption {
        description = "PostgreSQL container image.";
        type = types.str;
        default = "postgres:17-alpine";
      };

      user = mkOption {
        description = "PostgreSQL username.";
        type = types.str;
        default = "miniflux";
      };

      database = mkOption {
        description = "PostgreSQL database name.";
        type = types.str;
        default = "miniflux";
      };
    };

    image = mkOption {
      description = "Miniflux container image.";
      type = types.str;
      default = "miniflux/miniflux:latest";
    };

    adminUsername = mkOption {
      description = "Admin username for initial setup.";
      type = types.str;
      default = "admin";
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties.recordsize = "8K";

    # Create dedicated network for miniflux + its database
    modules.linux.oci.networks.${networkName}.enable = true;

    virtualisation.oci-containers.containers = {
      # PostgreSQL database container
      "miniflux_db" = {
        image = cfg.postgres.image;
        environment = {
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
        };
        # Password injected via environment file
        environmentFiles = [cfg.secrets.databasePasswordFile];
        volumes = [
          "${cfg.baseDir}:/var/lib/postgresql/data"
        ];
        extraOptions = [
          "--network-alias=miniflux_db"
          "--network=${ociLib.networkName networkName}"
          "--health-cmd=pg_isready -U ${cfg.postgres.user}"
          "--health-interval=10s"
          "--health-start-period=30s"
        ];
        log-driver = "journald";
      };

      # Miniflux application container
      "miniflux" = {
        image = cfg.image;
        dependsOn = ["miniflux_db"];
        environment =
          {
            "DATABASE_URL" = "postgres://${cfg.postgres.user}:$POSTGRES_PASSWORD@miniflux_db/${cfg.postgres.database}?sslmode=disable";
            "RUN_MIGRATIONS" = "1";
            "CREATE_ADMIN" = "1";
            "ADMIN_USERNAME" = cfg.adminUsername;
          }
          // optionalAttrs cfg.secrets.oidc.enable {
            "OAUTH2_PROVIDER" = "oidc";
            "OAUTH2_REDIRECT_URL" = cfg.secrets.oidc.redirectUrl;
            "OAUTH2_OIDC_DISCOVERY_ENDPOINT" = cfg.secrets.oidc.discoveryEndpoint;
            "OAUTH2_OIDC_PROVIDER_NAME" = cfg.secrets.oidc.providerName;
            "OAUTH2_USER_CREATION" =
              if cfg.secrets.oidc.userCreation
              then "1"
              else "0";
          };
        # Secrets injected via environment files
        environmentFiles =
          [
            cfg.secrets.databasePasswordFile
            cfg.secrets.adminPasswordFile
          ]
          ++ optionals (cfg.secrets.oidc.enable && cfg.secrets.oidc.clientIdFile != null) [
            cfg.secrets.oidc.clientIdFile
          ]
          ++ optionals (cfg.secrets.oidc.enable && cfg.secrets.oidc.clientSecretFile != null) [
            cfg.secrets.oidc.clientSecretFile
          ];
        ports = [
          "${toString cfg.port}:8080"
        ];
        extraOptions = [
          "--network-alias=miniflux"
          "--network=${ociLib.networkName networkName}"
        ];
        log-driver = "journald";
      };
    };

    # Systemd service configuration
    systemd.services = {
      "podman-miniflux_db" = ociLib.mkServiceConfig {
        networks = [networkName];
      };

      "podman-miniflux" = ociLib.mkServiceConfig {
        networks = [networkName];
        extraAfter = ["podman-miniflux_db.service"];
        extraRequires = ["podman-miniflux_db.service"];
      };
    };
  };
}
