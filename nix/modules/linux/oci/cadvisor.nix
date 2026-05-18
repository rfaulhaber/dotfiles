{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.cadvisor;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.cadvisor = {
    enable = mkEnableOption "cAdvisor container metrics exporter";

    image = imageLib.mkImageOptions {
      repository = "gcr.io/cadvisor/cadvisor";
      version = "v0.49.1";
    };

    port = mkOption {
      description = "Host port for the cAdvisor /metrics endpoint and UI.";
      type = types.port;
      default = 8090;
    };

    openFirewall = mkOption {
      description = ''
        Open the cAdvisor port in the host firewall. Required when
        Prometheus is on a different host. Leave false on the host that
        also runs Prometheus — the scrape can use the container network
        alias instead.
      '';
      type = types.bool;
      default = false;
    };

    networks = mkOption {
      description = ''
        Networks to join. Default joins the shared observability net so
        a co-located Prometheus can scrape via the `cadvisor` alias.
      '';
      type = types.listOf types.str;
      default = ["observability"];
    };

    enabledMetrics = mkOption {
      description = ''
        Comma-separated list of metric categories to enable. Default
        keeps the high-signal categories and drops the rest — cAdvisor's
        full metric set is enormous and most of it is noise.
      '';
      type = types.str;
      default = "cpu,cpuLoad,memory,network,disk,diskIO,oom_event";
    };

    housekeepingInterval = mkOption {
      description = "Cadence at which cAdvisor refreshes its container stats.";
      type = types.str;
      default = "10s";
    };

    podmanIntegration = mkOption {
      description = ''
        Bind-mount the host's Podman API socket into the container at
        /var/run/docker.sock so cAdvisor can resolve cgroup IDs to
        container names. Without this, podman containers appear as
        anonymous cgroup paths (`name` label is empty) because cAdvisor
        only natively talks Docker / containerd / k8s.

        Implies `virtualisation.podman.dockerSocket.enable = true` so
        the API socket is actually started.
      '';
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    virtualisation.podman.dockerSocket.enable = mkIf cfg.podmanIntegration true;

    virtualisation.oci-containers.containers.cadvisor = {
      image = imageLib.renderImage cfg.image;
      cmd = [
        "--housekeeping_interval=${cfg.housekeepingInterval}"
        "--enable_metrics=${cfg.enabledMetrics}"
        # Container labels balloon Prometheus cardinality without
        # adding much signal — turn them off and rely on `name`/`id`.
        "--store_container_labels=false"
      ];
      volumes =
        [
          "/:/rootfs:ro,rslave"
          "/sys:/sys:ro"
          "/var/lib/containers/storage:/var/lib/containers/storage:ro"
          "/dev/disk/:/dev/disk:ro"
        ]
        ++ (
          if cfg.podmanIntegration
          # cAdvisor v0.49+ has a NATIVE podman factory that talks the
          # podman REST API directly — not via the docker-compat surface.
          # It expects the socket at /var/run/podman/podman.sock inside
          # the container. Don't mount it to /var/run/docker.sock: the
          # docker factory then registers, tries to enumerate podman
          # cgroups using Docker's storage layout
          # (/var/lib/containers/storage/image/overlay/layerdb/...) which
          # podman doesn't use, and floods the logs with "failed to
          # identify the read-write layer ID" errors.
          then ["/run/podman/podman.sock:/var/run/podman/podman.sock:ro"]
          # Without podman integration, expose the host's /var/run so any
          # ambient docker.sock or runc state is reachable.
          else ["/var/run:/var/run:ro"]
        );
      ports = ["${toString cfg.port}:8080"];
      extraOptions =
        [
          "--network-alias=cadvisor"
          # cAdvisor needs broad host visibility (cgroups, /sys, /proc)
          # to enumerate containers. Privileged is the upstream-recommended
          # mode; narrowing it requires per-cap experimentation that's
          # not worth doing for an internal observability sidecar.
          "--privileged"
          # Podman defaults to cgroupns=private on cgroupv2, so each
          # container sees only its own cgroup as `/`. That hides every
          # sibling from cAdvisor and reduces metrics to a single root
          # series. Sharing the host cgroup namespace is the documented
          # fix; Docker defaults this way, which is why upstream's
          # example run command doesn't mention it.
          "--cgroupns=host"
          "--device=/dev/kmsg"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "cadvisor";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-cadvisor" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  };
}
