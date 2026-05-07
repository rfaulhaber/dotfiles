{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.pocket-id;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.pocket-id = {
    enable = mkEnableOption "Pocket-ID OIDC provider";

    appUrl = mkOption {
      description = "Public URL for Pocket-ID.";
      type = types.str;
      example = "https://auth.3679.space";
    };

    bindAddress = mkOption {
      description = "IP address to bind the published port to.";
      type = types.str;
      example = "66.63.168.244";
    };

    port = mkOption {
      description = "Host port for Pocket-ID.";
      type = types.port;
      default = 1411;
    };

    baseDir = mkOption {
      description = "Directory for Pocket-ID persistent data.";
      type = types.str;
      example = "/data/apps/pocket-id";
    };

    networks = mkOption {
      description = "Networks this container should join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/pocket-id/pocket-id";
      version = "latest";
    };

    user = {
      uid = mkOption {
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        type = types.int;
        default = 1000;
      };
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci.networks = mkIf (elem "default" cfg.networks) {
      default.enable = true;
    };

    sops.secrets."pocket-id/encryption-key" = {};

    sops.templates."pocket-id-env".content = ''
      ENCRYPTION_KEY=${config.sops.placeholder."pocket-id/encryption-key"}
    '';

    virtualisation.oci-containers.containers."pocket-id" = {
      image = imageLib.renderImage cfg.image;
      environment = {
        "APP_URL" = cfg.appUrl;
        "TRUST_PROXY" = "true";
        "PUID" = toString cfg.user.uid;
        "PGID" = toString cfg.user.gid;
      };
      environmentFiles = [
        config.sops.templates."pocket-id-env".path
      ];
      ports = [
        "${cfg.bindAddress}:${toString cfg.port}:1411"
      ];
      volumes = [
        "${cfg.baseDir}:/app/data:rw"
      ];
      extraOptions =
        ["--network-alias=pocket-id"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ [
          "--health-cmd=curl -f http://localhost:1411/healthz || exit 1"
          "--health-interval=90s"
          "--health-timeout=5s"
          "--health-retries=2"
          "--health-start-period=10s"
        ]
        ++ imageLib.mkImageLabels {
          module = "pocket-id";
          image = cfg.image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-pocket-id" = mkMerge [
      (ociLib.mkServiceConfig {
        networks = cfg.networks;
      })
      {
        serviceConfig.ExecStartPre = [
          "${pkgs.writeShellScript "pocket-id-dir-init" ''
            mkdir -p ${cfg.baseDir}
          ''}"
        ];
      }
    ];
  };
}
