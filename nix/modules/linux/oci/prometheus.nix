{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.prometheus;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.prometheus = {
    enable = mkEnableOption "Prometheus metrics server";

    image = imageLib.mkImageOptions {
      repository = "prom/prometheus";
      version = "v3.11.3";
    };

    baseDir = mkOption {
      description = "State directory for the Prometheus TSDB, mounted at /prometheus.";
      type = types.str;
      example = "/data/apps/prometheus";
    };

    port = mkOption {
      description = "Host port for the Prometheus UI / API.";
      type = types.port;
      default = 9090;
    };

    openFirewall = mkOption {
      description = ''
        Whether to open the Prometheus port in the host firewall.
        Prometheus only needs inbound for UI/API access — it scrapes
        targets outbound, so agents elsewhere don't need this on.
      '';
      type = types.bool;
      default = false;
    };

    networks = mkOption {
      description = "Networks to join.";
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

    retentionTime = mkOption {
      description = "How long to keep TSDB samples (--storage.tsdb.retention.time).";
      type = types.str;
      default = "30d";
    };

    scrapeInterval = mkOption {
      description = "Default scrape interval applied to all jobs.";
      type = types.str;
      default = "15s";
    };

    externalLabels = mkOption {
      description = "Labels added to every series leaving this Prometheus instance.";
      type = types.attrsOf types.str;
      default = {cluster = "home";};
    };

    extraScrapeConfigs = mkOption {
      description = ''
        Additional scrape_configs entries appended to the rendered
        prometheus.yml. Each entry is a job definition matching
        Prometheus' scrape_config schema.
      '';
      type = types.listOf types.attrs;
      default = [];
      example = literalExpression ''
        [
          {
            job_name = "node-atlas";
            static_configs = [{
              targets = ["host.containers.internal:9100"];
              labels.host = "atlas";
            }];
          }
        ]
      '';
    };

    user = {
      uid = mkOption {
        description = "UID inside the container. Default 65534 (nobody) matches the upstream prom/prometheus image.";
        type = types.int;
        default = 65534;
      };
      gid = mkOption {
        description = "GID inside the container.";
        type = types.int;
        default = 65534;
      };
    };

    configProperties = mkOption {
      description = ''
        ZFS properties applied to baseDir. recordsize=16K matches
        Prometheus' TSDB block layout — the default 128K causes severe
        read amplification on small range queries because every chunk
        read pulls a full 128K block off disk.
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "16K";};
    };
  };

  config = mkIf cfg.enable (let
    promConfig = {
      global = {
        scrape_interval = cfg.scrapeInterval;
        external_labels = cfg.externalLabels;
      };
      scrape_configs =
        [
          {
            job_name = "prometheus";
            static_configs = [{targets = ["localhost:9090"];}];
          }
        ]
        ++ cfg.extraScrapeConfigs;
    };
  in {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.templates."prometheus-config" = {
      content = builtins.toJSON promConfig;
      mode = "0444";
    };

    virtualisation.oci-containers.containers.prometheus = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {"TZ" = cfg.timezone;};
      volumes = [
        "${cfg.baseDir}:/prometheus"
        "${config.sops.templates."prometheus-config".path}:/etc/prometheus/prometheus.yml:ro"
      ];
      cmd = [
        "--config.file=/etc/prometheus/prometheus.yml"
        "--storage.tsdb.path=/prometheus"
        "--storage.tsdb.retention.time=${cfg.retentionTime}"
        "--web.console.libraries=/usr/share/prometheus/console_libraries"
        "--web.console.templates=/usr/share/prometheus/consoles"
        # Lifecycle endpoint enables `curl -X POST :9090/-/reload` so
        # config changes can be picked up without restarting the container.
        "--web.enable-lifecycle"
      ];
      ports = ["${toString cfg.port}:9090"];
      extraOptions =
        [
          "--network-alias=prometheus"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "prometheus";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-prometheus" = mkMerge [
      (ociLib.mkServiceConfig {
        networks = cfg.networks;
        sopsTemplates = ["prometheus-config"];
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
