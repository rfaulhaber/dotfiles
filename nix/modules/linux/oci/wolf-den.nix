{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.wolf-den;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
  # Bind sources under baseDir: podman won't create missing sources, and the
  # app — dropped to uid 1000 by the image entrypoint — must be able to
  # write all of them.
  stateDirs = ["${cfg.baseDir}/data" "${cfg.baseDir}/covers" "${cfg.baseDir}/compatibilitytools.d"];
in {
  options.modules.linux.oci.services.wolf-den = {
    enable = mkEnableOption "Wolf Den (web management UI for the Wolf streaming server)";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/games-on-whales/wolf-den";
      version = "stable";
    };

    baseDir = mkOption {
      description = ''
        Base directory for Wolf Den state: data/ holds the SQLite database —
        mounted at the path the app actually writes, /home/app/.local/share/
        wolf-den (the README's documented /app/wolf-den mount persists
        nothing) — covers/ downloaded game art, compatibilitytools.d/
        downloaded Proton builds.
      '';
      type = types.str;
      example = "/zroot/apps/wolf-den";
    };

    port = mkOption {
      description = "Host port for the web UI (container listens on 8080).";
      type = types.port;
      default = 8080;
    };

    bindAddress = mkOption {
      description = ''
        Host address the web UI publishes on. Wolf Den has no
        authentication of its own — any browser that reaches it holds
        Wolf's root-equivalent management API (pair clients, pull and run
        arbitrary images) — and podman's port publishing bypasses the
        NixOS firewall, so a wildcard bind exposes it regardless of
        firewall rules. Keep it on loopback (reach it via ssh -L) or a
        trusted overlay address.
      '';
      type = types.str;
      default = "127.0.0.1";
    };

    networks = mkOption {
      description = "Networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    extraEnv = mkOption {
      description = "Extra environment variables for the Wolf Den container.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.linux.oci.services.wolf.enable;
        message = "modules.linux.oci.services.wolf-den manages Wolf over its socket and requires services.wolf to be enabled.";
      }
    ];

    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    virtualisation.oci-containers.containers."wolf-den" = {
      image = imageLib.renderImage cfg.image;
      dependsOn = ["wolf"];
      environment =
        {
          # What the entrypoint's socat bridge connects to — must match the
          # host path of Wolf's socket, visible through the mount below.
          "WOLF_SOCKET_PATH" = "/tmp/sockets/wolf.sock";
        }
        // cfg.extraEnv;
      volumes = [
        # Parent dir rather than the socket file — same reasoning as the
        # wolf module: a socket-file bind goes stale when Wolf recreates it.
        "/tmp/sockets:/tmp/sockets:rw"
        "${cfg.baseDir}/data:/home/app/.local/share/wolf-den:rw"
        "${cfg.baseDir}/covers:/etc/wolf/covers:rw"
        "${cfg.baseDir}/compatibilitytools.d:/etc/wolf/compatibilitytools.d:rw"
      ];
      ports = ["${cfg.bindAddress}:${toString cfg.port}:8080"];
      log-driver = "journald";
      extraOptions =
        ["--network-alias=wolf-den"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "wolf-den";
          inherit (cfg) image;
        };
    };

    systemd.services."podman-wolf-den" = mkMerge [
      (ociLib.mkServiceConfig {
        inherit (cfg) networks;
      })
      {
        serviceConfig.ExecStartPre = [
          "${pkgs.coreutils}/bin/mkdir -p ${concatStringsSep " " stateDirs}"
          # The entrypoint chowns /home/app before dropping privileges but
          # leaves the /etc/wolf mounts alone; own them all for the app uid.
          "${pkgs.coreutils}/bin/chown 1000 ${concatStringsSep " " stateDirs}"
        ];
      }
    ];
  };
}
