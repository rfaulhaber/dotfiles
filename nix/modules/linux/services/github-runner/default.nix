# GitHub Actions self-hosted runners.
#
# Unlike the containerized Forgejo runners (modules.linux.oci.services.
# forgejo-runner), these run natively: jobs talk to the host nix daemon, so
# CI builds land in the host /nix/store where harmonia (modules.services.
# nix-cache) serves them to the rest of the fleet. That also means the host
# store is the warm cache between runs — no per-job store seeding or closure
# copying is needed.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.github-runner;
in {
  options.modules.services.github-runner = {
    enable = mkEnableOption "GitHub Actions self-hosted runners";

    url = mkOption {
      description = "Repository (or organization) URL the runners register with.";
      type = types.str;
      example = "https://github.com/rfaulhaber/dotfiles";
    };

    tokenFile = mkOption {
      description = ''
        Path to a file containing a raw fine-grained PAT with repository
        Administration read/write permission (a sops secret; not KEY=value
        format). The service exchanges it for short-lived registration
        tokens itself. It must be a PAT, not a registration token: ephemeral
        runners re-register after every job, and registration tokens expire
        after an hour.
      '';
      type = types.path;
      example = literalExpression ''config.sops.secrets."github-runner/token".path'';
    };

    count = mkOption {
      description = ''
        Number of runner instances to register. Each instance runs one job
        at a time, so this is the host's job concurrency — the analogue of
        the Forgejo runner's `capacity`. All instances share this host's
        CPU, RAM, and nix daemon.
      '';
      type = types.ints.positive;
      default = 1;
    };

    extraLabels = mkOption {
      description = "Labels the build matrix targets via runs-on, e.g. the arch label.";
      type = types.listOf types.str;
      default = [];
      example = ["nix-aarch64"];
    };
  };

  config = mkIf cfg.enable {
    services.github-runners = listToAttrs (map (i: {
        name = "${config.networking.hostName}-${toString i}";
        value = {
          enable = true;
          inherit (cfg) url tokenFile extraLabels;
          # De-register and wipe state after every job. The repo is public:
          # a workflow run should not leave anything behind for the next
          # one to find. Requires the PAT-based tokenFile above.
          ephemeral = true;
          replace = true;
          # bash, git, and the host nix are already on the unit PATH via
          # the upstream module; nu is for this repo's CI scripts.
          extraPackages = [pkgs.nushell];
        };
      })
      (range 1 cfg.count));
  };
}
