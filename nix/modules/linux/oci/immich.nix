{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.immich;
  ociLib = config.modules.linux.oci.lib;
  networkName = "immich";

  # Common env vars shared between server and ML containers
  dbEnv = {
    "DB_HOSTNAME" = "immich_postgres";
    "DB_USERNAME" = cfg.postgres.user;
    "DB_DATABASE_NAME" = cfg.postgres.database;
  };

  redisEnv = {
    "REDIS_HOSTNAME" = "immich_redis";
  };
  # ML image tag suffix based on GPU type
  mlImageSuffix = {
    nvidia = "-cuda";
    intel = "-openvino";
  };
  mlImage = let
    base = "ghcr.io/immich-app/immich-machine-learning:${cfg.version}";
    suffix = mlImageSuffix.${cfg.gpu} or "";
  in "${base}${suffix}";
in {
  options.modules.linux.oci.services.immich = {
    enable = mkEnableOption "Immich photo management";

    version = mkOption {
      description = "Immich version tag for server and ML images.";
      type = types.str;
      default = "release";
    };

    baseDir = mkOption {
      description = ''
        Parent directory for Immich state. Two children are bind-mounted
        into containers: <baseDir>/files (uploads, mounted at
        /usr/src/app/upload on immich-server) and <baseDir>/db (postgres,
        mounted at /var/lib/postgresql/data on immich_postgres).
      '';
      type = types.str;
      example = "/data/apps/immich";
    };

    filesProperties = mkOption {
      description = "ZFS properties applied to the files dataset.";
      type = types.attrsOf types.str;
      default = {};
    };

    dbProperties = mkOption {
      description = "ZFS properties applied to the db dataset. Defaults tuned for postgres.";
      type = types.attrsOf types.str;
      default = {recordsize = "8K";};
    };

    port = mkOption {
      description = "Host port for the Immich web interface.";
      type = types.port;
      default = 2283;
    };

    gpu = mkOption {
      description = "GPU type for machine learning container (null for CPU-only).";
      type = types.nullOr (types.enum ["nvidia" "intel"]);
      default = null;
    };

    postgres = {
      image = mkOption {
        description = "PostgreSQL container image (must include pgvecto.rs).";
        type = types.str;
        default = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0";
      };

      user = mkOption {
        description = "PostgreSQL username.";
        type = types.str;
        default = "immich";
      };

      database = mkOption {
        description = "PostgreSQL database name.";
        type = types.str;
        default = "immich";
      };
    };

    redis = {
      image = mkOption {
        description = "Redis/Valkey container image.";
        type = types.str;
        default = "valkey:8-bookworm";
      };
    };

    machineLearning = {
      enable = mkOption {
        description = "Enable the machine learning sidecar for smart search and face detection.";
        type = types.bool;
        default = true;
      };

      url = mkOption {
        description = ''
          External MACHINE_LEARNING_URL to delegate to (e.g. another host
          running an immich-machine-learning container). When non-null,
          immich-server uses this URL for ML calls. When null, immich-server
          uses its default (http://immich_machine_learning:3003), which only
          works when machineLearning.enable = true.
        '';
        type = types.nullOr types.str;
        default = null;
        example = "http://vulcan.lan:3003";
      };
    };
  };

  config = mkIf cfg.enable (let
    filesDir = "${cfg.baseDir}/files";
    dbDir = "${cfg.baseDir}/db";
  in {
    modules.linux.oci._managedPaths = {
      # Parent dataset has no mountpoint — only its children are mounted.
      "${cfg.baseDir}".properties.mountpoint = "none";
      ${filesDir}.properties = cfg.filesProperties;
      ${dbDir}.properties = cfg.dbProperties;
    };

    # Create dedicated network for immich services
    modules.linux.oci.networks.${networkName}.enable = true;

    # Named volume for ML model cache
    modules.linux.oci.volumes.immich_model_cache.enable =
      mkIf cfg.machineLearning.enable true;

    sops.secrets."immich/db-password" = {};

    # Shared by immich-server (DB_PASSWORD) and the postgres sidecar
    # (POSTGRES_PASSWORD). Both consume the same env file.
    sops.templates."immich-db-env".content = ''
      DB_PASSWORD=${config.sops.placeholder."immich/db-password"}
      POSTGRES_PASSWORD=${config.sops.placeholder."immich/db-password"}
    '';

    virtualisation.oci-containers.containers =
      {
        # PostgreSQL with pgvecto.rs
        "immich_postgres" = {
          image = cfg.postgres.image;
          environment = {
            "POSTGRES_USER" = cfg.postgres.user;
            "POSTGRES_DB" = cfg.postgres.database;
            "POSTGRES_INITDB_ARGS" = "--data-checksums";
          };
          environmentFiles = [config.sops.templates."immich-db-env".path];
          volumes = [
            "${dbDir}:/var/lib/postgresql/data"
          ];
          extraOptions = [
            "--network-alias=immich_postgres"
            "--network=${ociLib.networkName networkName}"
            "--health-cmd=pg_isready -d ${cfg.postgres.database} -U ${cfg.postgres.user}"
            "--health-interval=10s"
            "--health-start-period=30s"
          ];
          log-driver = "journald";
        };

        # Redis cache
        "immich_redis" = {
          image = cfg.redis.image;
          extraOptions = [
            "--network-alias=immich_redis"
            "--network=${ociLib.networkName networkName}"
            "--health-cmd=valkey-cli ping || exit 1"
            "--health-interval=10s"
            "--health-start-period=30s"
          ];
          log-driver = "journald";
        };

        # Immich server (API + web + microservices)
        "immich_server" = {
          image = "ghcr.io/immich-app/immich-server:${cfg.version}";
          dependsOn = ["immich_postgres" "immich_redis"];
          environment =
            dbEnv
            // redisEnv
            // optionalAttrs (cfg.machineLearning.url != null) {
              "MACHINE_LEARNING_URL" = cfg.machineLearning.url;
            };
          environmentFiles = [config.sops.templates."immich-db-env".path];
          volumes = [
            "${filesDir}:/usr/src/app/upload"
          ];
          ports = [
            "${toString cfg.port}:2283"
          ];
          extraOptions = [
            "--network-alias=immich_server"
            "--network=${ociLib.networkName networkName}"
          ];
          log-driver = "journald";
        };
      }
      // optionalAttrs cfg.machineLearning.enable {
        # Machine learning sidecar
        "immich_machine_learning" = {
          image = mlImage;
          volumes = [
            "${ociLib.volumeName "immich_model_cache"}:/cache"
          ];
          extraOptions =
            [
              "--network-alias=immich_machine_learning"
              "--network=${ociLib.networkName networkName}"
            ]
            ++ optionals (cfg.gpu == "nvidia") ["--device=nvidia.com/gpu=all"]
            ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri"];
          environment = optionalAttrs (cfg.gpu == "nvidia") {
            "NVIDIA_VISIBLE_DEVICES" = "all";
          };
          log-driver = "journald";
        };
      };

    # Systemd service configuration
    systemd.services =
      {
        "podman-immich_postgres" = ociLib.mkServiceConfig {
          networks = [networkName];
        };

        "podman-immich_redis" = ociLib.mkServiceConfig {
          networks = [networkName];
        };

        "podman-immich_server" = ociLib.mkServiceConfig {
          networks = [networkName];
          extraAfter = [
            "podman-immich_postgres.service"
            "podman-immich_redis.service"
          ];
          extraRequires = [
            "podman-immich_postgres.service"
            "podman-immich_redis.service"
          ];
        };
      }
      // optionalAttrs cfg.machineLearning.enable {
        "podman-immich_machine_learning" = ociLib.mkServiceConfig {
          networks = [networkName];
          volumes = ["immich_model_cache"];
        };
      };
  });
}
