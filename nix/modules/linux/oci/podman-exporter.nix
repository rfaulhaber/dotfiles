{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.podman-exporter;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.podman-exporter = {
    enable = mkEnableOption "Podman Prometheus exporter (prometheus-podman-exporter)";

    image = imageLib.mkImageOptions {
      repository = "quay.io/navidys/prometheus-podman-exporter";
      version = "v1.13.4";
    };

    port = mkOption {
      description = "Host port for the /metrics endpoint.";
      type = types.port;
      default = 9882;
    };

    openFirewall = mkOption {
      description = ''
        Open the exporter port in the host firewall. Required when
        Prometheus is on a different host. Leave false when Prometheus
        and the exporter share the observability network.
      '';
      type = types.bool;
      default = false;
    };

    networks = mkOption {
      description = ''
        Networks to join. Defaults to the observability network so a
        co-located Prometheus can scrape via the `podman-exporter` alias.
      '';
      type = types.listOf types.str;
      default = ["observability"];
    };

    collectors = mkOption {
      description = ''
        Comma-separated list of collectors to enable. The exporter has
        many; the default keeps the high-signal container-level metrics
        and skips per-pod/per-volume noise that explodes cardinality.
      '';
      type = types.str;
      default = "container,system";
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    # The exporter talks to the Podman REST API over its UNIX socket; the
    # socket service must be running. Don't gate this on an option — the
    # exporter is useless without it.
    virtualisation.podman.dockerSocket.enable = true;

    virtualisation.oci-containers.containers.podman-exporter = {
      image = imageLib.renderImage cfg.image;
      environment = {
        # Tell libpod-client which socket to use. The default search path
        # works too because we bind the socket at the canonical location,
        # but an explicit value makes intent clear in `podman inspect`.
        "CONTAINER_HOST" = "unix:///run/podman/podman.sock";
      };
      cmd = [
        "--collector.enable-all=false"
        "--web.listen-address=:9882"
      ];
      volumes = [
        # Read-only mount; the exporter only queries the API.
        "/run/podman/podman.sock:/run/podman/podman.sock:ro"
      ];
      ports = ["${toString cfg.port}:9882"];
      extraOptions =
        [
          "--network-alias=podman-exporter"
          # The exporter image runs as a non-root user by default. The
          # podman socket is owned by root:podman with mode 0660, so the
          # container process needs to be in the podman group OR run as
          # root. Easiest: run as root inside the container, which has
          # no effective privilege escalation because the container is
          # otherwise unprivileged and read-only mounts the socket.
          "--user=0:0"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "podman-exporter";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-podman-exporter" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  };
}
