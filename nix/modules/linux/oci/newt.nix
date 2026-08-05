{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.newt;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  # Only a unix:// endpoint needs the host socket bind-mounted in. tcp:// and
  # http(s):// endpoints are reached over a podman network instead, so the
  # container never sees a socket at all.
  socketIsUnix = cfg.dockerSocket != null && hasPrefix "unix://" cfg.dockerSocket;
  containerSocketPath = optionalString socketIsUnix (removePrefix "unix://" cfg.dockerSocket);
in {
  options.modules.linux.oci.services.newt = {
    enable = mkEnableOption "Newt tunnel agent (Pangolin)";

    image = imageLib.mkImageOptions {
      repository = "fosrl/newt";
      version = "latest";
    };

    pangolinEndpoint = mkOption {
      description = "URL of the Pangolin server to connect to.";
      type = types.str;
      example = "https://pangolin.example.com";
    };

    dockerSocket = mkOption {
      description = ''
        Endpoint newt queries to enumerate containers for Pangolin's target
        picker, as a *scheme-qualified* URL.

        `null` disables the integration: newt never starts its container event
        monitor and answers Pangolin's queries with `available: false`. The
        tunnel does not depend on this — connectivity is identical either way,
        and nothing is logged as an error.

        A `unix://` value bind-mounts `hostSocket` at that path. When
        `hostSocket` is the real podman socket this grants newt
        root-equivalent control of this host — a `:ro` mount does not
        constrain that: it marks the socket *inode* read-only, while the
        Docker API is a bidirectional conversation carried over the
        connection, and `POST /containers/create` with a host bind is a root
        shell. Prefer fronting it with `modules.services.docker-socket-proxy`
        and pointing `hostSocket` at its filtered socket — newt only ever
        issues GET /containers/json, GET /containers/{id}/json and
        GET /events.

        Note that newt enumerates *every* container on the host, not just those
        on `networks`: upstream gates that on DOCKER_ENFORCE_NETWORK_VALIDATION,
        which defaults off.
      '';
      type = types.nullOr types.str;
      default = null;
      example = "tcp://docker-socket-proxy:2375";
    };

    hostSocket = mkOption {
      description = ''
        Host socket bind-mounted into the container when `dockerSocket` is a
        `unix://` URL. Ignored for tcp:// endpoints. Point this at
        `modules.services.docker-socket-proxy`'s socketPath to give newt the
        filtered read-only view instead of the real socket.
      '';
      type = types.str;
      default = "/run/podman/podman.sock";
      example = "/run/docker-socket-proxy/docker.sock";
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

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = cfg.dockerSocket == null || hasInfix "://" cfg.dockerSocket;
          message = ''
            modules.linux.oci.services.newt.dockerSocket must carry a scheme
            ("tcp://host:2375", "unix:///var/run/docker.sock"). Newt rewrites a
            bare "host:port" into "unix://host:port" before connecting, which
            fails silently — the picker just reports no containers.
          '';
        }
      ];

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
        image = imageLib.renderImage cfg.image;
        environment =
          {
            "PANGOLIN_ENDPOINT" = cfg.pangolinEndpoint;
          }
          // optionalAttrs (cfg.dockerSocket != null) {
            "DOCKER_SOCKET" = cfg.dockerSocket;
          }
          // optionalAttrs (cfg.dns != null) {
            "DNS" = cfg.dns;
          };
        environmentFiles = [config.sops.templates."newt-env".path];
        volumes = optional socketIsUnix "${cfg.hostSocket}:${containerSocketPath}:ro";
        extraOptions =
          ["--network-alias=newt"]
          ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
          ++ imageLib.mkImageLabels {
            module = "newt";
            image = cfg.image;
          };
        log-driver = "journald";
      };

      systemd.services."podman-newt" = ociLib.mkServiceConfig {
        networks = cfg.networks;
        sopsTemplates = ["newt-env"];
      };
    }

    # Only the real podman socket needs its systemd unit; a proxy socket's
    # lifecycle belongs to the module providing it.
    (mkIf (socketIsUnix && cfg.hostSocket == "/run/podman/podman.sock") {
      systemd.sockets."podman".enable = true;
    })
  ]);
}
