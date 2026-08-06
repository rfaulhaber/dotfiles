{
  config,
  lib,
  pkgs,
  ...
}: {
  modules.linux.oci = {
    enable = true;
    # Image versions/digests come from oci-images.json so an
    # auto-update workflow can rewrite plain JSON instead of nix.
    services = lib.recursiveUpdate (lib.importJSON ./oci-images.json) {
      forgejo-runner = {
        enable = true;
        runners = {
          # Native aarch64 builder for the CI matrix. Hosts whose `runs-on`
          # matches the `nix-aarch64` label land here instead of going through
          # vulcan's binfmt emulation; for prometheus itself this also primes
          # the local /nix/store so nixos-rebuild on the host is a cache hit.
          default = {
            enable = true;
            instanceUrl = "http://git.home.lan";
            capacity = 2;
            jobStateDir = "/apps/forgejo-runner/default-state";
            tokenFile = config.sops.templates."forgejo-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix-aarch64:docker://nixos/nix:latest"
            ];
            baseDir = "/apps/forgejo-runner/default";
            validVolumes = [
              "/nix/var/nix/daemon-socket/socket"
            ];
            containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
          };
          codeberg = {
            enable = true;
            instanceUrl = "https://codeberg.org";
            capacity = 2;
            tokenFile = config.sops.templates."codeberg-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix-aarch64:docker://nixos/nix:latest"
            ];
            jobStateDir = "/apps/forgejo-runner/codeberg-state";
            baseDir = "/apps/forgejo-runner/codeberg";
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
