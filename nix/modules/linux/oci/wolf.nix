{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.wolf;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  appCatalog = import ./wolf-apps.nix;
  # Wire shape of Wolf's profile API: pin is a per-digit short array, keys
  # are snake_case. Rendered to JSON and handed to the sync script by path.
  profilesSpec =
    mapAttrsToList (id: p: {
      inherit id;
      inherit (p) name;
      pin =
        if p.pin == null
        then null
        else map toInt (stringToCharacters p.pin);
      icon_png_path = p.iconPngPath;
      extra_apps = p.extraApps;
      exclude_apps = p.excludeApps;
    })
    cfg.profiles;
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

    keepSessionImages = mkOption {
      description = ''
        Exempt Wolf's runtime-pulled session images (the GOW app images —
        steam, retroarch, es-de, pegasus — and Wolf UI) from the weekly
        `podman system prune --all` the OCI module schedules.

        Wolf pulls a session image only when it is missing and removes the
        session container when the app exits, so between plays every session
        image is unreferenced and the prune evicts the lot (~17 GB). The next
        launch then re-pulls a floating tag, and because the GOW images take
        Mesa from an unpinned PPA, a rebuilt `edge` can swap the GPU driver
        underneath Steam — invalidating every shader cache in appStateDir and
        forcing a full recompile on top of the multi-GB download. Keeping the
        images resident makes the driver stable; updating one becomes a
        deliberate `podman pull <image>` on the host (expect a one-time
        shader recompile when Mesa moves).
      '';
      type = types.bool;
      default = true;
    };

    profiles = mkOption {
      description = ''
        Wolf profiles to ensure exist, keyed by profile id. Profiles are app
        groupings picked in the in-session Wolf UI (the native Moonlight list
        shows only the special moonlight profile, whose default app is Wolf
        UI itself); apps launched through a profile keep state under a
        per-profile directory, so one person gets the same Steam login and
        library from any device they pair.

        Reconciled after service start through the management API rather
        than by templating config.toml, which Wolf rewrites on every
        pairing. Each profile starts from the image-default `user` profile's
        app list minus excludeApps plus includeApps/extraApps, and is only rebuilt when
        its declaration here changes — runtime edits made via the API or
        Wolf UI survive otherwise. Profiles removed from this set are left
        in place, not deleted. The baseline profile is cached beside the
        state file while it exists live, so `user` itself may be deleted
        from the picker without degrading later rebuilds.
      '';
      default = {};
      type = types.attrsOf (types.submodule ({
        name,
        config,
        ...
      }: {
        options = {
          name = mkOption {
            description = "Display name shown in the Wolf UI profile picker.";
            type = types.str;
            default = name;
          };

          pin = mkOption {
            description = ''
              Numeric PIN the Wolf UI asks for when opening the profile.
              A convenience gate only: it is enforced client-side and
              readable in plaintext through the management API and the nix
              store — not a security boundary.
            '';
            type = types.nullOr (types.strMatching "[0-9]+");
            default = null;
          };

          iconPngPath = mkOption {
            description = "Profile icon path as resolved inside the Wolf container.";
            type = types.nullOr types.str;
            default = null;
          };

          includeApps = mkOption {
            description = ''
              Names of catalog apps (see ./wolf-apps.nix) to append to this
              profile — shorthand for pasting the catalog entry into
              extraApps.
            '';
            type = types.listOf (types.enum (attrNames appCatalog));
            default = [];
          };

          extraApps = mkOption {
            description = ''
              Extra app definitions appended to this profile, in the JSON
              shape Wolf's /api/v1/profiles endpoints use. May be partial:
              required App fields nix can't know (resolved gstreamer
              pipelines, render_node) are completed from a live app record
              by the sync script, and a missing id derives from the title.
            '';
            type = types.listOf (types.attrsOf types.anything);
            default = [];
          };

          excludeApps = mkOption {
            description = "Titles of image-default apps to leave out of this profile.";
            type = types.listOf types.str;
            default = [];
          };
        };

        config.extraApps = map (n: appCatalog.${n}) config.includeApps;
      }));
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

    # Session images carry no tag the oci module pins, so the only handle on
    # them is the OCI source label they all share. `label!=` is podman's
    # negated filter: prune removes only images lacking the label, and with
    # several filters an image must lack all of them to go (libimage ANDs
    # same-key filters). The same filters reach container/network prune,
    # where they change nothing.
    virtualisation.podman.autoPrune.flags = mkIf cfg.keepSessionImages (
      concatMap (source: ["--filter" "label!=org.opencontainers.image.source=${source}"]) [
        "https://github.com/games-on-whales/gow"
        "https://github.com/games-on-whales/wolf-ui"
      ]
    );

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
          # Holds the pulse/wayland session sockets and the management API
          # socket ($XDG_RUNTIME_DIR/wolf.sock — /tmp/sockets/wolf.sock in
          # the container and on the host via the same-path mount). The API
          # is unauthenticated and root-equivalent (pairs clients, pulls and
          # runs arbitrary session images); socket file permissions are the
          # entire access control. Do NOT pin WOLF_SOCKET_PATH here: the
          # default Wolf UI app entry is generated with a hardcoded
          # /var/run/wolf/wolf.sock session mount, and Wolf only rewrites
          # that mount's source to the real socket when its own
          # WOLF_SOCKET_PATH env is absent — setting it severs the
          # in-session UI from the API ("failed to connect via localhost:80").
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
          inherit (cfg) image;
        };
    };

    systemd.services."podman-wolf" = ociLib.mkServiceConfig {
      networks = [];
      extraAfter = ["podman.socket"];
      extraRequires = ["podman.socket"];
    };

    # Profile reconciliation is deliberately best-effort (the script always
    # exits 0): it runs during activation, and a sync hiccup must not fail —
    # and thereby roll back — a whole deploy. Check its journal if a declared
    # profile doesn't show up in the Wolf UI.
    systemd.services."wolf-profiles-sync" = mkIf (cfg.profiles != {}) {
      description = "Reconcile declared Wolf profiles via the management API";
      wantedBy = ["multi-user.target"];
      after = ["podman-wolf.service"];
      requires = ["podman-wolf.service"];
      path = [pkgs.curl];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = let
          syncScript =
            lib.my.writeNushellScriptBin pkgs "wolf-profiles-sync"
            (builtins.readFile ./wolf-profiles-sync.nu);
          specFile = pkgs.writeText "wolf-profiles.json" (builtins.toJSON profilesSpec);
        in "${syncScript}/bin/wolf-profiles-sync --file ${specFile} --state-file ${cfg.baseDir}/nix-profiles-last-applied.json --template-cache-file ${cfg.baseDir}/nix-baseline-template.json --socket /tmp/sockets/wolf.sock";
      };
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
