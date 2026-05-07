{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.immich;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
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
  # ML image: the GPU suffix is part of the upstream tag scheme
  # (release-cuda, release-openvino), so it goes between version and any
  # optional digest — not after the digest, since the digest pins the
  # already-suffixed manifest.
  mlImage = let
    img = cfg.machineLearning.image;
    suffix = mlImageSuffix.${cfg.gpu} or "";
  in
    "${img.repository}:${img.version}${suffix}"
    + optionalString (img.digest != null) "@${img.digest}";
in {
  options.modules.linux.oci.services.immich = {
    enable = mkEnableOption "Immich photo management";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/immich-app/immich-server";
      version = "release";
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
      description = ''
        ZFS properties applied to the files dataset. When filesEncryption.enable
        is true, encryption-related properties (encryption, keyformat,
        keylocation, canmount=noauto) are merged in automatically — anything
        set here wins on conflict.
      '';
      type = types.attrsOf types.str;
      default = {};
    };

    dbProperties = mkOption {
      description = "ZFS properties applied to the db dataset. Defaults tuned for postgres.";
      type = types.attrsOf types.str;
      default = {recordsize = "8K";};
    };

    filesEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the files dataset using a sops-managed key. Sets up a
          dedicated systemd unit that loads the key from
          `config.sops.secrets."immich/zfs-key".path` after sops has
          rendered it, then mounts the dataset before immich_server starts.
          The dataset is given canmount=noauto so the early
          `zfs-mount.service` skips it.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key. Treated as `format = "binary"` by sops-nix,
          rendered to `/run/secrets/immich-zfs-key` at activation time.
        '';
        type = types.path;
      };
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
      image = imageLib.mkImageOptions {
        repository = "ghcr.io/immich-app/postgres";
        version = "14-vectorchord0.4.3-pgvectors0.2.0";
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
      # The canonical valkey image is at valkey/valkey, not the
      # docker-hub library namespace (valkey:* doesn't exist on
      # docker.io/library; podman would 404 trying to pull it).
      image = imageLib.mkImageOptions {
        repository = "valkey/valkey";
        version = "8-bookworm";
      };
    };

    machineLearning = {
      image = imageLib.mkImageOptions {
        repository = "ghcr.io/immich-app/immich-machine-learning";
        version = "release";
      };

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
    # Dataset name = mountpoint with leading slash stripped, matching the
    # convention enforced by modules.linux.oci._managedPaths.
    filesDataset = removePrefix "/" filesDir;
    keyFilePath = config.sops.secrets."immich/zfs-key".path or "/run/secrets/immich-zfs-key";
    encryptionProperties = optionalAttrs cfg.filesEncryption.enable {
      encryption = "aes-256-gcm";
      keyformat = "raw";
      keylocation = "file://${keyFilePath}";
      # Skip the early zfs-mount.service — the unlock unit handles it.
      canmount = "noauto";
    };
  in {
    modules.linux.oci._managedPaths = {
      # Parent dataset has no mountpoint — only its children are mounted.
      "${cfg.baseDir}".properties.mountpoint = "none";
      # Encryption properties go first so user-supplied filesProperties wins on conflict.
      ${filesDir}.properties = encryptionProperties // cfg.filesProperties;
      ${dbDir}.properties = cfg.dbProperties;
    };

    # Wire the unlock unit when encryption is enabled. The unit's `before`
    # ordering against zfs-manage-datasets.service ensures `recordsize`
    # tweaks land on an unlocked dataset; `consumers` makes podman-immich_server
    # wait for the unlock without callers needing extraAfter/extraRequires.
    modules.services.zfs.encryptedDatasets = mkIf cfg.filesEncryption.enable {
      immich = {
        dataset = filesDataset;
        keyFile = keyFilePath;
        consumers = ["podman-immich_server.service"];
      };
    };

    # Create dedicated network for immich services
    modules.linux.oci.networks.${networkName}.enable = true;

    # Named volume for ML model cache
    modules.linux.oci.volumes.immich_model_cache.enable =
      mkIf cfg.machineLearning.enable true;

    sops.secrets =
      {
        "immich/db-password" = {};
      }
      // optionalAttrs cfg.filesEncryption.enable {
        # The whole sops file is treated as opaque binary — `keyformat = raw`
        # means ZFS expects exactly 32 bytes, so any text-mode round-trip
        # would corrupt it.
        "immich/zfs-key" = {
          format = "binary";
          sopsFile = cfg.filesEncryption.keyFile;
        };
      };

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
          image = imageLib.renderImage cfg.postgres.image;
          environment = {
            "POSTGRES_USER" = cfg.postgres.user;
            "POSTGRES_DB" = cfg.postgres.database;
            "POSTGRES_INITDB_ARGS" = "--data-checksums";
          };
          environmentFiles = [config.sops.templates."immich-db-env".path];
          volumes = [
            "${dbDir}:/var/lib/postgresql/data"
          ];
          extraOptions =
            [
              "--network-alias=immich_postgres"
              "--network=${ociLib.networkName networkName}"
              "--health-cmd=pg_isready -d ${cfg.postgres.database} -U ${cfg.postgres.user}"
              "--health-interval=10s"
              "--health-start-period=30s"
            ]
            ++ imageLib.mkImageLabels {
              module = "immich.postgres";
              image = cfg.postgres.image;
            };
          log-driver = "journald";
        };

        # Redis cache
        "immich_redis" = {
          image = imageLib.renderImage cfg.redis.image;
          extraOptions =
            [
              "--network-alias=immich_redis"
              "--network=${ociLib.networkName networkName}"
              "--health-cmd=valkey-cli ping || exit 1"
              "--health-interval=10s"
              "--health-start-period=30s"
            ]
            ++ imageLib.mkImageLabels {
              module = "immich.redis";
              image = cfg.redis.image;
            };
          log-driver = "journald";
        };

        # Immich server (API + web + microservices)
        "immich_server" = {
          image = imageLib.renderImage cfg.image;
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
          extraOptions =
            [
              "--network-alias=immich_server"
              "--network=${ociLib.networkName networkName}"
            ]
            ++ imageLib.mkImageLabels {
              module = "immich";
              image = cfg.image;
            };
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
            ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri"]
            ++ imageLib.mkImageLabels {
              module = "immich.machineLearning";
              image = cfg.machineLearning.image;
            };
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
          # zfs-load-key-immich.service ordering is wired via the
          # encryptedDatasets `consumers` field above.
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
