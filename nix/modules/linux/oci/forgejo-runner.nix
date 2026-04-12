{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.forgejo-runner;
  ociLib = config.modules.linux.oci.lib;

  runnerOpts = {name, ...}: {
    options = {
      enable = mkEnableOption "Forgejo runner '${name}'";

      image = mkOption {
        description = "Forgejo runner container image.";
        type = types.str;
        default = "code.forgejo.org/forgejo/runner:6.2.2";
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
    };
  };

  enabledRunners = filterAttrs (_: v: v.enable) cfg.runners;

  mkRunnerContainer = name: runnerCfg: let
    labelsStr = concatStringsSep "," runnerCfg.labels;
  in {
    image = runnerCfg.image;
    environment = {
      "DOCKER_HOST" = "unix:///var/run/docker.sock";
      "FORGEJO_INSTANCE" = runnerCfg.instanceUrl;
    };
    environmentFiles = [runnerCfg.tokenFile];
    volumes = [
      "${runnerCfg.baseDir}:/data"
      "/run/podman/podman.sock:/var/run/docker.sock"
    ];
    entrypoint = "/bin/sh";
    cmd = [
      "-c"
      ''
        if [ ! -f /data/.runner ]; then
          forgejo-runner register \
            --instance "$FORGEJO_INSTANCE" \
            --token "$FORGEJO_TOKEN" \
            --name "${runnerCfg.runnerName}" \
            --labels "${labelsStr}" \
            --no-interactive
        fi
        exec forgejo-runner daemon
      ''
    ];
    extraOptions =
      ["--network-alias=forgejo-runner-${name}"]
      ++ (map (n: "--network=${ociLib.networkName n}") runnerCfg.networks);
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
    # Ensure the Podman socket is available for runners to spawn job containers
    systemd.sockets."podman".enable = true;

    modules.linux.oci.networks = mkMerge (mapAttrsToList (_: runnerCfg:
      mkIf (elem "default" runnerCfg.networks) {
        default.enable = true;
      }
    ) enabledRunners);

    virtualisation.oci-containers.containers =
      mapAttrs' (name: runnerCfg:
        nameValuePair "forgejo-runner-${name}" (mkRunnerContainer name runnerCfg)
      ) enabledRunners;

    systemd.services = mapAttrs' (name: runnerCfg:
      nameValuePair "podman-forgejo-runner-${name}" (ociLib.mkServiceConfig {
        networks = runnerCfg.networks;
      })
    ) enabledRunners;
  };
}
