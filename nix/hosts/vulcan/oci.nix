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
      newt = {
        enable = true;
        pangolinEndpoint = "https://pangolin.3679.space";
        dns = "192.168.0.2";
      };
      podman-exporter = {
        enable = true;
        # Open so atlas's Prometheus can scrape over the LAN at :9882.
        openFirewall = true;
      };

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
          codeberg = {
            enable = true;
            instanceUrl = "https://codeberg.org";
            capacity = 4;
            tokenFile = config.sops.templates."codeberg-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix:docker://nixos/nix:latest"
            ];
            jobStateDir = "/zroot/apps/forgejo-runner/codeberg-state";
            baseDir = "/zroot/apps/forgejo-runner/codeberg";
            validVolumes = [
              "/nix/var/nix/daemon-socket/socket"
            ];
            containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
          };
        };
      };
    };
  };
}
