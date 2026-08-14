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
      version = "v1.21.0";
      digest = "sha256:2ebb9e09101d8cc1e28e3f306b56a722450918e628208435201ed39bd62403cb";
    };

    port = mkOption {
      description = "Host port for the /metrics endpoint.";
      type = types.port;
      default = 9882;
    };

    bindAddress = mkOption {
      description = ''
        Host address to publish the metrics port on. `null` binds all
        interfaces (0.0.0.0) — appropriate on a trusted LAN where the
        host firewall gates access. Set to a specific address (e.g. a
        VPN overlay IP) on multi-homed or public hosts: podman publishes
        host ports via a nat/PREROUTING DNAT that bypasses the NixOS
        firewall, so `networking.firewall` cannot narrow this port —
        scoping the publish to one address is the only way to keep it off
        untrusted interfaces.
      '';
      type = types.nullOr types.str;
      default = null;
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

        The `container` collector is always on (the exporter enables it by
        default and exposes no flag to toggle it), so it is rendered as a
        no-op here — listing it is harmless. Every other name maps to a
        `--collector.<name>` flag.
      '';
      type = types.str;
      default = "container,system";
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    # When bindAddress points at a VPN overlay IP, that address can be absent
    # while the unit starts: a nixos-rebuild restarts the VPN client in the
    # same switch, and podman's host-port reservation then fails with
    # EADDRNOTAVAIL, failing the unit and (under deploy-rs) rolling back the
    # whole deploy. Worse, on hosts where the overlay's control plane itself
    # runs in containers here, the address only returns minutes later — after
    # the full container stack converges — so no start-ordering or retry
    # window can bridge it. Allow binding non-local addresses instead; the
    # listener simply receives no traffic until the interface is back.
    # Loopback is exempt: 127.0.0.0/8 is always bindable, so a loopback
    # bindAddress shouldn't loosen this host-wide sysctl for nothing.
    boot.kernel.sysctl = mkIf (cfg.bindAddress != null && !(hasPrefix "127." cfg.bindAddress)) {
      "net.ipv4.ip_nonlocal_bind" = 1;
    };

    # The exporter reads the Podman REST API at /run/podman/podman.sock.
    # podman.socket serves that path whenever virtualisation.podman is
    # enabled (which the oci stack guarantees) — deliberately NOT setting
    # dockerSocket.enable: its only effect is a host-wide /run/docker.sock
    # compat symlink onto the same socket, which nothing here consumes.

    virtualisation.oci-containers.containers.podman-exporter = {
      image = imageLib.renderImage cfg.image;
      environment = {
        # Tell libpod-client which socket to use. The default search path
        # works too because we bind the socket at the canonical location,
        # but an explicit value makes intent clear in `podman inspect`.
        "CONTAINER_HOST" = "unix:///run/podman/podman.sock";
      };
      cmd =
        [
          "--collector.enable-all=false"
          "--web.listen-address=:9882"
        ]
        # `container` is enabled by default and has no flag; every other
        # requested collector needs an explicit `--collector.<name>`.
        # Without this the `system` (etc.) metrics the option promises are
        # never scraped.
        ++ (map (c: "--collector.${c}") (
          filter (c: c != "container") (splitString "," cfg.collectors)
        ));
      volumes = [
        # :ro protects only the socket inode — the API behind it is fully
        # read-write and a rootful podman socket is host-root-equivalent
        # (it can start privileged containers). This mount IS a deliberate
        # grant of that power to the exporter; the flag is not a safeguard.
        "/run/podman/podman.sock:/run/podman/podman.sock:ro"
      ];
      ports = [
        (
          if cfg.bindAddress == null
          then "${toString cfg.port}:9882"
          else "${cfg.bindAddress}:${toString cfg.port}:9882"
        )
      ];
      extraOptions =
        [
          "--network-alias=podman-exporter"
          # The exporter image runs as a non-root user by default. The
          # podman socket is owned by root:podman with mode 0660, so the
          # container process needs to be in the podman group OR run as
          # root. Run as root inside the container: it adds nothing on
          # top of what the socket mount already grants — holding the
          # rootful socket is host-root-equivalent regardless of the
          # container's own user.
          "--user=0:0"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "podman-exporter";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-podman-exporter" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  };
}
