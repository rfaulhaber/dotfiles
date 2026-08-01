{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.wolf;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.wolf = {
    enable = mkEnableOption "Wolf (Games on Whales) Moonlight game streaming server";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/games-on-whales/wolf";
      version = "stable";
    };

    baseDir = mkOption {
      description = ''
        Wolf's identity directory: config.toml (generated on first boot), TLS
        keypair, and paired-client certs. Mounted over appStateDir's cfg/
        subpath — see the volume list for why. Apps and gstreamer pipelines
        are configured in config.toml — treated as runtime state, not managed
        declaratively.
      '';
      type = types.str;
      example = "/zroot/apps/wolf";
    };

    appStateDir = mkOption {
      description = ''
        Host directory holding per-app session state (Proton prefixes, Steam
        home, shader caches). Wolf creates session-container bind mounts and
        writes its fake-udev helper using this path *as seen by the container
        runtime on the host*, so it is mounted into the Wolf container at the
        identical path. Must not be on a noexec mount.
      '';
      type = types.str;
      example = "/store/games/state";
    };

    gpu = mkOption {
      description = "GPU type for hardware encoding (null falls back to software x264).";
      type = types.nullOr (types.enum ["nvidia" "intel"]);
      default = null;
    };

    renderNode = mkOption {
      description = "DRM render node Wolf's GStreamer encoder uses.";
      type = types.str;
      default = "/dev/dri/renderD128";
    };

    logLevel = mkOption {
      description = "Wolf log verbosity.";
      type = types.enum ["TRACE" "DEBUG" "INFO" "WARNING" "ERROR"];
      default = "INFO";
    };

    openFirewall = mkOption {
      description = "Whether to open the Moonlight protocol ports.";
      type = types.bool;
      default = false;
    };

    extraEnv = mkOption {
      description = "Extra environment variables for the Wolf container.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    # Wolf spawns one sibling container per streaming session over the Docker
    # HTTP API. Podman's compat socket implements the surface Wolf uses
    # (create/start/stop/wait/logs/pull, API v1.40), so no dockerd is needed —
    # but it must be the rootful socket: session containers get real devices
    # (/dev/dri, virtual input). Session images (ghcr.io/games-on-whales/
    # steam:edge etc.) are pulled by Wolf at runtime and are NOT covered by
    # oci-images.json pinning.
    systemd.sockets.podman.wantedBy = ["sockets.target"];

    boot.kernelModules = ["uinput" "uhid"];

    # Podman refuses to start a container whose bind-mount source is missing
    # (docker would mkdir it), and /tmp is a fresh tmpfs every boot — so the
    # session-socket dir has to be guaranteed host-side. NixOS activation runs
    # tmpfiles before restarting units, covering deploys as well as boots.
    systemd.tmpfiles.rules = ["d /tmp/sockets 0755 root root -"];

    # Upstream-recommended rules: uinput/uhid access for virtual device
    # creation, and seat9 assignment so a local session never grabs a remote
    # player's virtual keyboard/mouse/pad.
    services.udev.extraRules = ''
      KERNEL=="uinput", SUBSYSTEM=="misc", MODE="0660", GROUP="input", OPTIONS+="static_node=uinput"
      KERNEL=="uhid", TAG+="uaccess"
      SUBSYSTEMS=="input", ATTRS{id/vendor}=="ab00", MODE="0660", GROUP="input", ENV{ID_SEAT}="seat9"
      SUBSYSTEMS=="input", ATTRS{name}=="Wolf X-Box One (virtual) pad", MODE="0660", GROUP="input"
      SUBSYSTEMS=="input", ATTRS{name}=="Wolf PS5 (virtual) pad", MODE="0660", GROUP="input"
      SUBSYSTEMS=="input", ATTRS{name}=="Wolf gamepad (virtual) motion sensors", MODE="0660", GROUP="input"
      SUBSYSTEMS=="input", ATTRS{name}=="Wolf Nintendo (virtual) pad", MODE="0660", GROUP="input"
    '';

    virtualisation.oci-containers.containers."wolf" = {
      image = imageLib.renderImage cfg.image;
      environment =
        {
          "WOLF_LOG_LEVEL" = cfg.logLevel;
          "WOLF_RENDER_NODE" = cfg.renderNode;
          "WOLF_DOCKER_SOCKET" = "/run/podman/podman.sock";
          # Resolved on the host when Wolf creates session-container mounts —
          # must match the host-side path of the same-path volume below.
          "HOST_APPS_STATE_FOLDER" = cfg.appStateDir;
          "XDG_RUNTIME_DIR" = "/tmp/sockets";
        }
        // optionalAttrs (cfg.gpu == "nvidia") {
          "NVIDIA_VISIBLE_DEVICES" = "all";
          "NVIDIA_DRIVER_CAPABILITIES" = "all";
        }
        // cfg.extraEnv;
      volumes = [
        # Parent dir rather than the socket file: a socket-file bind mount goes
        # stale if systemd recreates the socket; the dir mount survives.
        "/run/podman:/run/podman:rw"
        # Whole /dev plus udev data: Wolf resolves device majors and forwards
        # hotplug (controller connect) into running session containers.
        "/dev:/dev:rw"
        "/run/udev:/run/udev:rw"
        # Same-path mounts: Wolf shares these into session containers by host
        # path (app state + pulse/wayland sockets), so in-container and host
        # paths must be identical.
        "${cfg.appStateDir}:${cfg.appStateDir}:rw"
        # The image's startup.sh unconditionally exports the config location
        # as $HOST_APPS_STATE_FOLDER/cfg — env overrides are clobbered — so
        # keeping identity (uuid, certs, paired clients) on a durable pool
        # while bulk app state sits on the disposable games pool has to be
        # expressed as a nested mount. Host-side, cfg/ under appStateDir is
        # just an empty mount-scaffold dir.
        "${cfg.baseDir}:${cfg.appStateDir}/cfg:rw"
        "/tmp/sockets:/tmp/sockets:rw"
      ];
      log-driver = "journald";
      extraOptions =
        [
          # Moonlight pairing advertises the host IP and per-session RTP ports,
          # and client discovery relies on mDNS — bridge networking breaks both.
          "--network=host"
          # Upstream's podman deployment adds these: shared IPC for the
          # gstreamer/pulse shm paths, and the input-major cgroup rule so
          # hotplugged controller nodes (c 13:*) are usable, since --device
          # only covers nodes present at container start.
          "--ipc=host"
          "--device-cgroup-rule=c 13:* rmw"
          "--device=/dev/uinput"
          "--device=/dev/uhid"
        ]
        ++ optionals (cfg.gpu == "intel") ["--device=/dev/dri:/dev/dri"]
        ++ optionals (cfg.gpu == "nvidia") ["--device=nvidia.com/gpu=all"]
        ++ imageLib.mkImageLabels {
          module = "wolf";
          image = cfg.image;
        };
    };

    systemd.services."podman-wolf" = ociLib.mkServiceConfig {
      networks = [];
      extraAfter = ["podman.socket"];
      extraRequires = ["podman.socket"];
    };

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [
        47984 # HTTPS pairing/protocol
        47989 # HTTP pairing/serverinfo
        48010 # RTSP session setup
      ];
      allowedUDPPorts = [
        47999 # ENet control stream
      ];
      # One video+audio port pair per concurrent session, counting up from the
      # base; ranges give headroom for ~10 simultaneous streams.
      allowedUDPPortRanges = [
        {
          from = 48100; # video RTP
          to = 48110;
        }
        {
          from = 48200; # audio RTP
          to = 48210;
        }
      ];
    };
  };
}
