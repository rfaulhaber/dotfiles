{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.seerr;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.seerr = {
    enable = mkEnableOption "Seerr media request manager";

    image = imageLib.mkImageOptions {
      repository = "ghcr.io/seerr-team/seerr";
      version = "v3";
    };

    baseDir = mkOption {
      description = ''
        Base directory for seerr state (mounted at /app/config): settings.json,
        the SQLite database and logs. settings.json is written by the setup
        wizard and holds media-server tokens and *arr API keys in plaintext.
      '';
      type = types.str;
      example = "/data/apps/seerr";
    };

    webPort = mkOption {
      description = "Host port for the seerr web UI.";
      type = types.port;
      default = 5055;
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    networks = mkOption {
      description = "Networks to join.";
      type = types.listOf types.str;
      default = ["default"];
    };

    dependsOn = mkOption {
      description = "Other oci-containers this service depends on.";
      type = types.listOf types.str;
      default = [];
    };

    extraEnv = mkOption {
      description = "Additional environment variables (e.g. a LOG_LEVEL override).";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    virtualisation.oci-containers.containers.seerr = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          TZ = cfg.timezone;
          # Upstream defaults to debug, which floods the journal.
          LOG_LEVEL = "info";
        }
        // cfg.extraEnv;
      # `:U` chowns the dataset to the image's baked-in node user (1000:1000).
      # Nothing in the image fixes ownership at start-up, and the ZFS dataset
      # is otherwise root-owned, so the first settings.json write would fail.
      volumes = ["${cfg.baseDir}:/app/config:U"];
      ports = ["${toString cfg.webPort}:5055"];
      extraOptions =
        ["--network-alias=seerr"]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ [
          # The image runs npm as PID 1 and ships no init of its own;
          # upstream requires one for signal handling and zombie reaping.
          "--init"
          "--cap-drop=all"
          "--security-opt=no-new-privileges:true"
        ]
        ++ imageLib.mkImageLabels {
          module = "seerr";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-seerr" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
    };
  };
}
