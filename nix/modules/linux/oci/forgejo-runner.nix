{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.forgejo-runner;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  runnerOpts = {name, ...}: {
    options = {
      enable = mkEnableOption "Forgejo runner '${name}'";

      image = imageLib.mkImageOptions {
        repository = "code.forgejo.org/forgejo/runner";
        version = "6.2.2";
      };

      instanceUrl = mkOption {
        description = "URL of the Forgejo instance to register with.";
        type = types.str;
        example = "https://forgejo.example.com";
      };

      tokenFile = mkOption {
        description = ''
          Path to environment file containing the runner registration token (sops secret).
          Must be in KEY=value format with FORGEJO_TOKEN=<token>.
        '';
        type = types.path;
        example = literalExpression "config.sops.secrets.forgejo-runner-token.path";
      };

      runnerName = mkOption {
        description = "Display name for the runner in Forgejo.";
        type = types.str;
        default = "${config.networking.hostName}-${name}";
      };

      labels = mkOption {
        description = "Runner labels in 'name:execution_method://image' format.";
        type = types.listOf types.str;
        example = [
          "docker:docker://node:20-bookworm"
          "ubuntu-latest:docker://ubuntu:latest"
        ];
      };

      capacity = mkOption {
        description = ''
          Number of jobs this runner can execute in parallel. Matrix strategies
          only run concurrently if the runner's capacity is at least as large as
          the number of matrix legs you want running at once. All concurrent jobs
          share this host's CPU, RAM, and nix daemon — tune based on resources.
        '';
        type = types.ints.positive;
        default = 1;
      };

      baseDir = mkOption {
        description = "Base directory for persistent runner data.";
        type = types.str;
        example = "/data/apps/forgejo-runner/x86_64";
      };

      networks = mkOption {
        description = "Networks this container should join.";
        type = types.listOf types.str;
        default = ["default"];
      };

      validVolumes = mkOption {
        description = "Host paths that job containers are allowed to mount via workflow container.volumes.";
        type = types.listOf types.str;
        default = [];
        example = ["/nix/var/nix/daemon-socket/socket"];
      };

      containerOptions = mkOption {
        description = "Extra Docker/Podman options applied to all job containers created by this runner.";
        type = types.str;
        default = "";
        example = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
      };

      jobStateDir = mkOption {
        description = ''
          Optional host directory bind-mounted into every job container at
          /ci-state. Intended for workflows that want to persist small
          warm-cache state (e.g. nix store seed paths) across runs without
          committing it to git. The directory is registered as a managed path
          (ZFS dataset when OCI ZFS is enabled); job scripts create whatever
          subdirectory structure they need under /ci-state.
        '';
        type = types.nullOr types.str;
        default = null;
        example = "/apps/forgejo-runner/default/state";
      };
    };
  };

  # Fold jobStateDir into the runner's effective container options so it
  # appears in every job container spawned by that runner. forgejo-runner
  # additionally validates every -v mount in `options` against the
  # `valid_volumes` whitelist, so the path must be added to both lists.
  effectiveContainerOptions = runnerCfg:
    runnerCfg.containerOptions
    + (optionalString (runnerCfg.jobStateDir != null)
      " -v ${runnerCfg.jobStateDir}:/ci-state");

  effectiveValidVolumes = runnerCfg:
    runnerCfg.validVolumes
    ++ (optional (runnerCfg.jobStateDir != null) runnerCfg.jobStateDir);

  enabledRunners = filterAttrs (_: v: v.enable) cfg.runners;

  mkRunnerContainer = name: runnerCfg: let
    labelsStr = concatStringsSep "," runnerCfg.labels;
    configFile = pkgs.writeText "forgejo-runner-${name}-config.yaml" (builtins.toJSON {
      runner = {
        capacity = runnerCfg.capacity;
      };
      container = {
        valid_volumes = effectiveValidVolumes runnerCfg;
        options = effectiveContainerOptions runnerCfg;
        docker_host = "unix:///var/run/docker.sock";
      };
    });
  in {
    image = imageLib.renderImage runnerCfg.image;
    user = "0:0";
    environment = {
      "DOCKER_HOST" = "unix:///var/run/docker.sock";
      "FORGEJO_INSTANCE" = runnerCfg.instanceUrl;
    };
    environmentFiles = [runnerCfg.tokenFile];
    volumes = [
      "${runnerCfg.baseDir}:/data"
      "/run/podman/podman.sock:/var/run/docker.sock"
      "${configFile}:/config.yaml:ro"
    ];
    entrypoint = "/bin/sh";
    cmd = [
      "-c"
      ''
        if [ ! -f /data/.runner ]; then
          forgejo-runner register \
            --config /config.yaml \
            --instance "$FORGEJO_INSTANCE" \
            --token "$FORGEJO_TOKEN" \
            --name "${runnerCfg.runnerName}" \
            --labels "${labelsStr}" \
            --no-interactive
        fi
        exec forgejo-runner daemon --config /config.yaml
      ''
    ];
    extraOptions =
      ["--network-alias=forgejo-runner-${name}"]
      ++ (map (n: "--network=${ociLib.networkName n}") runnerCfg.networks)
      ++ imageLib.mkImageLabels {
        module = "forgejo-runner.${name}";
        image = runnerCfg.image;
      };
    log-driver = "journald";
  };
in {
  options.modules.linux.oci.services.forgejo-runner = {
    enable = mkEnableOption "Forgejo CI runners";

    runners = mkOption {
      type = types.attrsOf (types.submodule runnerOpts);
      default = {};
      description = "Attrset of Forgejo runner instances.";
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths = let
      basePaths =
        mapAttrs' (
          _: r:
            nameValuePair r.baseDir {}
        )
        enabledRunners;
      statePaths = listToAttrs (
        mapAttrsToList
        (_: r: nameValuePair r.jobStateDir {})
        (filterAttrs (_: r: r.jobStateDir != null) enabledRunners)
      );
    in
      basePaths // statePaths;

    # Ensure the Podman socket is available for runners to spawn job containers
    systemd.sockets."podman".enable = true;

    modules.linux.oci.networks = mkMerge (mapAttrsToList (
        _: runnerCfg:
          mkIf (elem "default" runnerCfg.networks) {
            default.enable = true;
          }
      )
      enabledRunners);

    virtualisation.oci-containers.containers =
      mapAttrs' (
        name: runnerCfg:
          nameValuePair "forgejo-runner-${name}" (mkRunnerContainer name runnerCfg)
      )
      enabledRunners;

    systemd.services =
      mapAttrs' (
        name: runnerCfg:
          nameValuePair "podman-forgejo-runner-${name}" (ociLib.mkServiceConfig {
            networks = runnerCfg.networks;
          })
      )
      enabledRunners;
  };
}
