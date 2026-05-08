{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.miniflux;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
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

    oidc = {
      enable = mkEnableOption ''
        OIDC authentication. When true, the module requires sops secrets at
        "miniflux/oidc-client-id" and "miniflux/oidc-client-secret"
      '';

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

    postgres = {
      image = imageLib.mkImageOptions {
        repository = "postgres";
        version = "18-alpine";
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

      pgdata = mkOption {
        description = ''
          PGDATA path inside the container (set as env var). Default
          matches the postgres image's standard layout. Override when
          the bind-mounted volume has data nested inside subdirectories
          (some legacy installs nest under /<major>/docker/).
        '';
        type = types.str;
        default = "/var/lib/postgresql/data";
        example = "/var/lib/postgresql/data/18/docker";
      };
    };

    image = imageLib.mkImageOptions {
      repository = "miniflux/miniflux";
      version = "latest";
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

    sops.secrets =
      {
        "miniflux/db-password" = {};
        "miniflux/admin-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "miniflux/oidc-client-id" = {};
        "miniflux/oidc-client-secret" = {};
      };

    sops.templates =
      {
        # Shared by miniflux and the postgres sidecar. POSTGRES_PASSWORD
        # is consumed by postgres on initdb (no-op afterwards). DATABASE_URL
        # is consumed by miniflux — must be fully rendered here, with the
        # placeholder substituted, because podman doesn't do shell-style
        # variable expansion in env values (docker-compose did).
        "miniflux-db-env".content = ''
          POSTGRES_PASSWORD=${config.sops.placeholder."miniflux/db-password"}
          DATABASE_URL=postgres://${cfg.postgres.user}:${config.sops.placeholder."miniflux/db-password"}@miniflux_db/${cfg.postgres.database}?sslmode=disable
        '';
        "miniflux-admin-env".content = ''
          ADMIN_PASSWORD=${config.sops.placeholder."miniflux/admin-password"}
        '';
      }
      // optionalAttrs cfg.oidc.enable {
        "miniflux-oidc-env".content = ''
          OAUTH2_CLIENT_ID=${config.sops.placeholder."miniflux/oidc-client-id"}
          OAUTH2_CLIENT_SECRET=${config.sops.placeholder."miniflux/oidc-client-secret"}
        '';
      };

    virtualisation.oci-containers.containers = {
      # PostgreSQL database container
      "miniflux_db" = {
        image = imageLib.renderImage cfg.postgres.image;
        environment = {
          "POSTGRES_USER" = cfg.postgres.user;
          "POSTGRES_DB" = cfg.postgres.database;
          "PGDATA" = cfg.postgres.pgdata;
        };
        # Password injected via environment file
        environmentFiles = [config.sops.templates."miniflux-db-env".path];
        volumes = [
          "${cfg.baseDir}:/var/lib/postgresql/data"
        ];
        extraOptions =
          [
            "--network-alias=miniflux_db"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "miniflux.postgres";
            image = cfg.postgres.image;
          };
        log-driver = "journald";
      };

      # Miniflux application container
      "miniflux" = {
        image = imageLib.renderImage cfg.image;
        dependsOn = ["miniflux_db"];
        environment =
          {
            # DATABASE_URL is provided via the sops-rendered env file
            # (miniflux-db-env) so the password placeholder gets properly
            # substituted at activation. Putting it here would pass
            # `$POSTGRES_PASSWORD` literally because podman doesn't expand
            # shell-style variable references.
            "RUN_MIGRATIONS" = "1";
            "CREATE_ADMIN" = "1";
            "ADMIN_USERNAME" = cfg.adminUsername;
          }
          // optionalAttrs cfg.oidc.enable {
            "OAUTH2_PROVIDER" = "oidc";
            "OAUTH2_REDIRECT_URL" = cfg.oidc.redirectUrl;
            "OAUTH2_OIDC_DISCOVERY_ENDPOINT" = cfg.oidc.discoveryEndpoint;
            "OAUTH2_OIDC_PROVIDER_NAME" = cfg.oidc.providerName;
            "OAUTH2_USER_CREATION" =
              if cfg.oidc.userCreation
              then "1"
              else "0";
          };
        environmentFiles =
          [
            config.sops.templates."miniflux-db-env".path
            config.sops.templates."miniflux-admin-env".path
          ]
          ++ optional cfg.oidc.enable config.sops.templates."miniflux-oidc-env".path;
        ports = [
          "${toString cfg.port}:8080"
        ];
        extraOptions =
          [
            "--network-alias=miniflux"
            "--network=${ociLib.networkName networkName}"
          ]
          ++ imageLib.mkImageLabels {
            module = "miniflux";
            image = cfg.image;
          };
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
