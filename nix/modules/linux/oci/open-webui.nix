{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.open-webui;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.open-webui = {
    enable = mkEnableOption "Open WebUI (frontend for Ollama and OpenAI-compatible APIs)";

    image = mkOption {
      description = "Open WebUI container image.";
      type = types.str;
      default = "ghcr.io/open-webui/open-webui:main";
    };

    baseDir = mkOption {
      description = "Base directory for Open WebUI persistent state.";
      type = types.str;
      example = "/apps/open-webui";
    };

    port = mkOption {
      description = "Host port for the Open WebUI HTTP server.";
      type = types.port;
      default = 3000;
    };

    ollamaBaseUrl = mkOption {
      description = ''
        Base URL of the Ollama backend. Defaults to the in-network alias used
        by the Ollama OCI module on the shared default network.
      '';
      type = types.str;
      default = "http://ollama:11434";
    };

    networks = mkOption {
      description = "Networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    openFirewall = mkOption {
      description = "Whether to open the web UI port to the host firewall.";
      type = types.bool;
      default = false;
    };

    extraEnvironment = mkOption {
      description = "Additional environment variables for the Open WebUI container.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    virtualisation.oci-containers.containers."open-webui" = {
      image = cfg.image;
      environment =
        {
          "OLLAMA_BASE_URL" = cfg.ollamaBaseUrl;
        }
        // cfg.extraEnvironment;
      volumes = [
        "${cfg.baseDir}:/app/backend/data:rw"
      ];
      ports = [
        "${toString cfg.port}:8080"
      ];
      log-driver = "journald";
      extraOptions =
        ["--network-alias=open-webui"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
    };

    systemd.services."podman-open-webui" = mkMerge [
      (ociLib.mkServiceConfig {
        networks = cfg.networks;
      })
      {
        serviceConfig.ExecStartPre = ["${pkgs.coreutils}/bin/mkdir -p ${cfg.baseDir}"];
      }
    ];

    networking.firewall = mkIf cfg.openFirewall {
      allowedTCPPorts = [cfg.port];
    };
  };
}
