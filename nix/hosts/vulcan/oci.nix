{
  config,
  lib,
  ...
}: {
  modules.linux.oci = {
    enable = true;
    zfs = {
      enable = true;
      pool = "zroot";
    };
    registryAuth = {
      enable = true;
      # The same forgejo registry atlas reaches as localhost:2835, here via
      # its public name — credentials are keyed by the literal host string in
      # the image ref, so each name needs its own entry even for one server.
      # The secret is base64("<user>:<PAT with read:packages>").
      registries."git.3679.space".secret = "registry-auth/forgejo";
    };
    # Image versions/digests come from oci-images.json so an
    # auto-update workflow can rewrite plain JSON instead of nix.
    services = lib.recursiveUpdate (lib.importJSON ./oci-images.json) {
      plex = {
        enable = true;
        baseDir = "/zroot/apps/plex";
        gpu = "intel";
        openFirewall = true;
        mediaDirs = {
          movies = "/mnt/media/movies";
          tv = "/mnt/media/tv";
        };
      };
      jellyfin = {
        enable = true;
        baseDir = "/zroot/apps/jellyfin";
        gpu = "intel";
        openFirewall = true;
        tvDir = "/mnt/media/tv";
        moviesDir = "/mnt/media/movies";
      };
      immich-ml = {
        enable = true;
        gpu = "intel";
        openFirewall = true;
      };
      open-webui = {
        enable = true;
        baseDir = "/zroot/apps/open-webui";
        openFirewall = true;
        # Native ollama runs on the host (modules.services.ollama), so
        # reach it via the podman-injected host gateway DNS entry.
        ollamaBaseUrl = "http://host.containers.internal:11434";
      };
      newt = {
        enable = true;
        pangolinEndpoint = "https://pangolin.3679.space";
        dns = "192.168.0.2";
        # The filtered proxy socket, never the real one — direct access is
        # root-equivalent, and on vulcan the blast radius includes the binary
        # cache via the trust-boundary note below.
        dockerSocket = "unix:///var/run/docker.sock";
        hostSocket = config.modules.services.docker-socket-proxy.socketPath;
      };
      podman-exporter = {
        enable = true;
        # Open so atlas's Prometheus can scrape over the LAN at :9882.
        openFirewall = true;
      };

      tumblr-alt-text-bot = {
        enable = true;
        baseDir = "/zroot/apps/tumblr-alt-text-bot";
        botBlog = "alt-text-bot";
      };

      # TRUST BOUNDARY: like the forgejo runners below, Wolf holds the rootful
      # podman socket — anyone who completes Moonlight pairing can trigger
      # session containers that run with real device access. Pairing requires
      # PIN confirmation against Wolf's local API, and the protocol ports are
      # LAN-only (openFirewall, no tunnel exposure) — keep it that way.
      wolf = {
        enable = true;
        baseDir = "/zroot/apps/wolf";
        # store pool (games NVMe): Proton prefixes and shader caches land on
        # the small-record dataset. The 1M-record /store/games/steam dataset is
        # for the Steam library; point Steam at it via the app's mounts in
        # config.toml once Wolf has generated it — at a subpath like
        # /home/retro/games, NOT /home/retro itself: Wolf implicitly mounts
        # the app state folder there and podman rejects the duplicate mount
        # destination that docker tolerates (wolf issue #461).
        appStateDir = "/store/games/state";
        gpu = "intel";
        openFirewall = true;
        # Heroic's library, on the 1M-record dataset below rather than the
        # 128K appStateDir it would otherwise inherit. Deliberately a
        # sibling of Heroic's Prefixes/ dir instead of a mount over
        # Games/Heroic itself: wine prefixes are thousands of small files
        # rewritten during play and belong on the small record size.
        # Heroic's own "Default Install Path" has to point here — it is
        # runtime state in config.json, not something nix sets.
        appMounts.heroic = ["/store/games/heroic:/home/retro/Games/Heroic/Library:rw"];

        # Picked inside the Wolf UI session; each profile keeps its own
        # Steam login and per-app state, shared across every device that
        # person pairs. PINs can be added later via `pin = "...."` — they
        # gate the picker UI only, nothing cryptographic.
        profiles = {
          ryan = {
            includeApps = ["heroic"];
          };
          juni = {};
        };
      };

      # Fronts wolf's unauthenticated root-equivalent socket, so it stays
      # loopback-only; reach it with: ssh -L 8080:127.0.0.1:8080 vulcan
      wolf-den = {
        enable = true;
        baseDir = "/zroot/apps/wolf-den";
      };

      # SECURITY TRUST BOUNDARY: every runner here grants its job containers
      # the host Nix daemon socket, and the runner daemon itself holds the
      # podman socket (needed to spawn job containers). A job that runs as
      # root therefore connects to the Nix daemon as a *trusted* user — it can
      # import arbitrary store paths that harmonia then re-signs and serves to
      # atlas/prometheus, i.e. it can poison the LAN binary cache. Nothing in
      # Nix gates this; the only control is that workflow runs from untrusted
      # contributors require maintainer approval before they execute. That is
      # enforced forge-side, not here. Before adding a runner against any forge
      # where external contributors can trigger CI without approval, this model
      # breaks — isolate that runner (drop the host nix-daemon socket, or front
      # the podman socket with a restricting proxy) rather than reusing this.
      forgejo-runner = {
        enable = true;
        runners = {
          default = {
            enable = true;
            # vulcan and atlas are on the same LAN
            instanceUrl = "http://git.home.lan";
            # Allow up to 4 concurrent jobs so CI workflows that use
            # strategy.matrix (e.g. per-host NixOS builds) can run in parallel.
            capacity = 4;
            # Bind-mounted into every job container at /ci-state. Used by the
            # flake-update workflow to persist per-host seed paths for warming
            # the container /nix/store on the next run.
            jobStateDir = "/zroot/apps/forgejo-runner/default-state";
            tokenFile = config.sops.templates."forgejo-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix:docker://nixos/nix:latest"
            ];
            baseDir = "/zroot/apps/forgejo-runner/default";
            validVolumes = [
              "/nix/var/nix/daemon-socket/socket"
            ];
            containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
          };
        };
      };
    };
  };

  # Read-only container visibility for newt's target picker: list containers
  # and watch start/stop, nothing else. The forgejo-runner above keeps the
  # real socket — it genuinely needs write access to spawn job containers.
  modules.services.docker-socket-proxy = {
    enable = true;
    allowedApiSections = ["containers" "events"];
  };

  # Game content for the Heroic session app: large files read sequentially,
  # so the same 1M record as store/games/steam. Declared here rather than in
  # disko.nix because disko only runs from an installer — zfs-manage-datasets
  # creates this on the live host during activation, and adopts it on a
  # from-scratch rebuild. Owned by ryan (uid 1000) to match the uid Wolf
  # lobby sessions run as; a root-owned mount source would be read-only to
  # the container.
  modules.services.zfs.datasets."store/games/heroic" = {
    properties = {
      mountpoint = "/store/games/heroic";
      recordsize = "1M";
    };
    owner = "ryan";
    group = "users";
    mode = "0755";
  };

  # Ordering only — the bind-mount source must exist when podman-newt starts
  # (podman errors on a missing volume source rather than creating it).
  # Deliberately no Requires=: the picker is optional, the tunnel is not, and
  # if the proxy is down newt's Restart=always retries cover the gap.
  systemd.services."podman-newt".after = ["docker-socket-proxy.service"];
}
