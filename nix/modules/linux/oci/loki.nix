{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.loki;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.loki = {
    enable = mkEnableOption "Loki log aggregation";

    image = imageLib.mkImageOptions {
      repository = "grafana/loki";
      version = "3.7.1";
    };

    baseDir = mkOption {
      description = ''
        State directory for Loki, mounted at /loki. Holds chunks,
        indexes, WAL, and compactor working dir.
      '';
      type = types.str;
      example = "/data/apps/loki";
    };

    port = mkOption {
      description = "Host port for the Loki HTTP API (push + query).";
      type = types.port;
      default = 3100;
    };

    openFirewall = mkOption {
      description = ''
        Whether to open the Loki port in the host firewall. Required if
        agents on other hosts (Promtail, Alloy, Vector) need to push
        logs to this Loki instance over the LAN.
      '';
      type = types.bool;
      default = false;
    };

    networks = mkOption {
      description = "Networks to join. Default joins the shared observability net so Grafana can reach it by alias.";
      type = types.listOf types.str;
      default = ["observability"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    retentionPeriod = mkOption {
      description = ''
        How long to keep log lines (Go duration string). 720h = 30d
        is a sensible default for a homelab.
      '';
      type = types.str;
      default = "720h";
    };

    user = {
      uid = mkOption {
        description = "UID inside the container. Default 10001 matches the upstream grafana/loki image.";
        type = types.int;
        default = 10001;
      };
      gid = mkOption {
        description = "GID inside the container.";
        type = types.int;
        default = 10001;
      };
    };

    configProperties = mkOption {
      description = ''
        ZFS properties applied to baseDir. Loki writes large append-only
        chunk files; recordsize=128K matches its typical chunk size and
        gives ZFS room to compress the LZ4-compressed chunks well.
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "128K";};
    };
  };

  config = mkIf cfg.enable (let
    # Single-binary mode with filesystem storage. TSDB index + filesystem
    # object store is the lightest viable Loki config. Schema v13 is the
    # current recommended schema for Loki 3.x.
    lokiConfig = {
      auth_enabled = false;
      server = {
        http_listen_port = 3100;
        grpc_listen_port = 9096;
        log_level = "info";
      };
      common = {
        path_prefix = "/loki";
        storage.filesystem = {
          chunks_directory = "/loki/chunks";
          rules_directory = "/loki/rules";
        };
        replication_factor = 1;
        ring.kvstore.store = "inmemory";
      };
      schema_config.configs = [
        {
          from = "2024-01-01";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }
      ];
      limits_config = {
        retention_period = cfg.retentionPeriod;
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
        allow_structured_metadata = true;
      };
      compactor = {
        working_directory = "/loki/compactor";
        retention_enabled = true;
        delete_request_store = "filesystem";
      };
      analytics.reporting_enabled = false;
    };
  in {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.templates."loki-config" = {
      content = builtins.toJSON lokiConfig;
      mode = "0444";
    };

    virtualisation.oci-containers.containers.loki = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {"TZ" = cfg.timezone;};
      volumes = [
        "${cfg.baseDir}:/loki"
        # Loki's default CMD is `-config.file=/etc/loki/local-config.yaml`,
        # so dropping our config there means we don't need to override cmd.
        "${config.sops.templates."loki-config".path}:/etc/loki/local-config.yaml:ro"
      ];
      ports = ["${toString cfg.port}:3100"];
      extraOptions =
        [
          "--network-alias=loki"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "loki";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-loki" = mkMerge [
      (ociLib.mkServiceConfig {
        inherit (cfg) networks;
        sopsTemplates = ["loki-config"];
      })
      {
        serviceConfig.ExecStartPre = [
          "+${pkgs.coreutils}/bin/install -d -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} -m 0755 ${cfg.baseDir}"
        ];
      }
    ];

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  });
}
