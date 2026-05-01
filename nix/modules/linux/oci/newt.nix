{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.newt;
  ociLib = config.modules.linux.oci.lib;
in {
  options.modules.linux.oci.services.newt = {
    enable = mkEnableOption "Newt tunnel agent (Pangolin)";

    image = mkOption {
      description = "Newt container image.";
      type = types.str;
      default = "fosrl/newt";
    };

    pangolinEndpoint = mkOption {
      description = "URL of the Pangolin server to connect to.";
      type = types.str;
      example = "https://pangolin.example.com";
    };

    dns = mkOption {
      description = "Custom DNS server for the container.";
      type = types.nullOr types.str;
      default = null;
      example = "192.168.0.2";
    };

    networks = mkOption {
      description = "Podman networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };
  };

  config = mkIf cfg.enable {
    # Ensure the Podman socket is available for newt to discover containers
    systemd.sockets."podman".enable = true;

    # Enable any referenced networks
    modules.linux.oci.networks = mkMerge (map (n: {
        ${n}.enable = true;
      })
      cfg.networks);

    sops.secrets = {
      "newt/id" = {};
      "newt/secret" = {};
    };

    sops.templates."newt-env".content = ''
      NEWT_ID=${config.sops.placeholder."newt/id"}
      NEWT_SECRET=${config.sops.placeholder."newt/secret"}
    '';

    virtualisation.oci-containers.containers."newt" = {
      image = cfg.image;
      environment =
        {
          "PANGOLIN_ENDPOINT" = cfg.pangolinEndpoint;
          "DOCKER_SOCKET" = "unix:///var/run/docker.sock";
        }
        // optionalAttrs (cfg.dns != null) {
          "DNS" = cfg.dns;
        };
      environmentFiles = [config.sops.templates."newt-env".path];
      volumes = [
        "/run/podman/podman.sock:/var/run/docker.sock:ro"
      ];
      extraOptions =
        ["--network-alias=newt" "--no-healthcheck"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
      log-driver = "journald";
    };

    systemd.services."podman-newt" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
