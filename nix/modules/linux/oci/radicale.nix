{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.radicale;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  settingsFormat = pkgs.formats.ini {};
  # Listen addresses come from the image's CMD (0.0.0.0 + [::]), so the
  # config only carries auth and storage. No secrets in here — the htpasswd
  # file is referenced by path, so plain writeText-style generation is fine.
  configFile = settingsFormat.generate "radicale-config" (recursiveUpdate {
      auth = {
        type = "htpasswd";
        htpasswd_filename = "/etc/radicale/users";
        # The official image installs Radicale[bcrypt]; autodetect verifies
        # bcrypt (and sha512 etc.) hashes without pinning one scheme.
        htpasswd_encryption = "autodetect";
      };
      storage.filesystem_folder = "/var/lib/radicale/collections";
    }
    cfg.settings);
in {
  options.modules.linux.oci.services.radicale = {
    enable = mkEnableOption "Radicale CalDAV/CardDAV server";

    image = imageLib.mkImageOptions {
      # :latest tracks nightly on this image — always pin a release tag.
      repository = "ghcr.io/kozea/radicale";
      version = "3.7.8";
    };

    baseDir = mkOption {
      description = ''
        Radicale state directory, mounted at /var/lib/radicale. Collections
        (calendars/addressbooks, one file per item) live in a `collections`
        subdirectory Radicale creates itself.
      '';
      type = types.str;
      example = "/data/apps/radicale";
    };

    port = mkOption {
      description = "Host port for the DAV endpoint and built-in web UI.";
      type = types.port;
      default = 5232;
    };

    user = {
      uid = mkOption {
        description = "UID to run the container as (radicale uses --user, not PUID).";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID to run the container as.";
        type = types.int;
        default = 100;
      };
    };

    settings = mkOption {
      description = ''
        Radicale INI config as section → key → value, merged over the module
        defaults (htpasswd auth, filesystem storage). See
        https://radicale.org/v3.html#configuration for the full surface —
        e.g. rights.type for cross-user sharing policies.
      '';
      inherit (settingsFormat) type;
      default = {};
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
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Collections are many small files.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir}.properties = cfg.configProperties;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    # Full htpasswd file (user:bcrypt-hash lines). World-readable so the
    # container's non-root user can read it through the bind mount — it
    # holds password hashes, not plaintext. The restart matters because
    # podman resolves the /run/secrets symlink at mount time: after a
    # rotation the running container still sees the old generation.
    sops.secrets."radicale/users" = {
      mode = "0444";
      restartUnits = ["podman-radicale.service"];
    };

    virtualisation.oci-containers.containers.radicale = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = cfg.extraEnv;
      volumes = [
        "${cfg.baseDir}:/var/lib/radicale"
        "${configFile}:/etc/radicale/config:ro"
        "${config.sops.secrets."radicale/users".path}:/etc/radicale/users:ro"
      ];
      ports = ["${toString cfg.port}:5232"];
      extraOptions =
        [
          "--network-alias=radicale"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "radicale";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-radicale" = mkMerge [
      (ociLib.mkServiceConfig {
        inherit (cfg) networks;
      })
      {
        # A fresh dataset mounts root:root; the container user must own it
        # to create collections/. install -d is idempotent and the leading
        # `+` runs it as root.
        serviceConfig.ExecStartPre = [
          "+${pkgs.coreutils}/bin/install -d -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} -m 0750 ${cfg.baseDir}"
        ];
      }
    ];
  };
}
