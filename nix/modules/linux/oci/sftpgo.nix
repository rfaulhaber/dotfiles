{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.sftpgo;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.sftpgo = {
    enable = mkEnableOption "SFTPGo file server (WebDAV + web UI, optional SFTP)";

    image = imageLib.mkImageOptions {
      repository = "drakkan/sftpgo";
      version = "v2.7.5-alpine";
    };

    baseDir = mkOption {
      description = ''
        State directory, mounted at /var/lib/sftpgo — the image's WORKDIR
        and home. SFTPGo's configDir defaults to ".", so the SQLite
        provider DB and auto-generated SSH host keys resolve here (not
        /etc/sftpgo); losing it regenerates host keys and drops all
        users/admins.
      '';
      type = types.str;
      example = "/data/apps/sftpgo";
    };

    dataDir = mkOption {
      description = ''
        users_base_dir, mounted at /srv/sftpgo/data. Per-user home
        directories are created beneath it; point individual users at
        other mounts (extraVolumes) via their home_dir or virtual
        folders in the admin UI.
      '';
      type = types.str;
      example = "/data/apps/sftpgo/data";
    };

    webPort = mkOption {
      description = "Port for the web UIs + REST API — host publish and container listen.";
      type = types.port;
      default = 6580;
    };

    davPort = mkOption {
      description = "Port for the WebDAV binding — host publish and container listen.";
      type = types.port;
      default = 6581;
    };

    sftpPort = mkOption {
      description = "Port for the SFTP binding; 0 (default) disables SFTP entirely.";
      type = types.port;
      default = 0;
    };

    adminUsername = mkOption {
      description = ''
        Default admin auto-created on first start (password from the sops
        secret "sftpgo/admin-password"). Creation only runs while the
        provider has no admins, so the env staying set is harmless.
      '';
      type = types.str;
      default = "admin";
    };

    trustedProxies = mkOption {
      description = ''
        Peers allowed to assert X-Forwarded-For toward the WebDAV
        binding. Keep it to the podman networks where newt delivers
        tunneled traffic.
      '';
      type = types.listOf types.str;
      default = ["10.89.0.0/16"];
    };

    oidc = {
      enable = mkOption {
        description = ''
          OIDC login for the web UIs (WebAdmin + WebClient share one
          client per binding). Requires sops secrets
          "sftpgo/oidc-client-id" and "sftpgo/oidc-client-secret".
          Protocol logins (WebDAV/SFTP) keep using provider passwords.
        '';
        type = types.bool;
        default = false;
      };

      configUrl = mkOption {
        description = "OIDC issuer URL (SFTPGo appends the well-known discovery path).";
        type = types.str;
        example = "https://auth.example.com";
      };

      redirectBaseUrl = mkOption {
        description = "Public base URL of this SFTPGo instance, used to build the OIDC redirect URI.";
        type = types.str;
        example = "https://drive.example.com";
      };

      usernameField = mkOption {
        description = "OIDC claim to map to the SFTPGo username.";
        type = types.str;
        default = "preferred_username";
      };
    };

    defender.enable = mkOption {
      description = ''
        SFTPGo's built-in brute-force defender — off upstream by default.
        Scores failed logins per client IP and temp-bans past a
        threshold; required armor before exposing the WebDAV binding
        publicly. Real client IPs come from the trustedProxies +
        X-Forwarded-For wiring, so bans don't key on the tunnel address.
      '';
      type = types.bool;
      default = true;
    };

    user = {
      uid = mkOption {
        description = "UID the container runs as (via --user; the image defaults to 1000:1000).";
        type = types.int;
        default = 1000;
      };
      gid = mkOption {
        description = "GID the container runs as (via --user).";
        type = types.int;
        default = 100;
      };
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
      description = "Additional environment variables (SFTPGO_* keys configure the server).";
      type = types.attrsOf types.str;
      default = {};
    };

    extraVolumes = mkOption {
      description = ''
        Additional bind mounts (e.g. a shared backup dataset mounted as a
        user's home). Sources must exist and be writable by the container
        user before the container starts.
      '';
      type = types.listOf types.str;
      default = [];
    };

    configEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the baseDir dataset (SQLite provider DB — user/admin
          records and password hashes — plus the SSH host keys) using a
          sops-managed raw key. The dataset is marked canmount=noauto and
          unlocked by a dedicated systemd unit before podman-sftpgo starts.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key for the baseDir dataset.
        '';
        type = types.path;
      };
    };

    dataEncryption = {
      enable = mkOption {
        description = ''
          Encrypt the dataDir dataset (per-user home directories) using a
          sops-managed raw key. Same wiring as configEncryption.
        '';
        type = types.bool;
        default = false;
      };

      keyFile = mkOption {
        description = ''
          Path to a sops-encrypted binary file containing the raw 32-byte
          ZFS encryption key for the dataDir dataset.
        '';
        type = types.path;
      };
    };

    configProperties = mkOption {
      description = ''
        ZFS properties applied to the baseDir dataset. Defaults tuned for
        SQLite. When configEncryption.enable is true, encryption-related
        properties (encryption, keyformat, keylocation, canmount=noauto)
        are merged in automatically — anything set here wins on conflict.
      '';
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };

    dataProperties = mkOption {
      description = "ZFS properties applied to the dataDir dataset.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable (let
    configKeyPath =
      config.sops.secrets."sftpgo/config-zfs-key".path
      or "/run/secrets/sftpgo-config-zfs-key";
    dataKeyPath =
      config.sops.secrets."sftpgo/data-zfs-key".path
      or "/run/secrets/sftpgo-data-zfs-key";

    mkEncryptionProps = keyPath: {
      encryption = "aes-256-gcm";
      keyformat = "raw";
      keylocation = "file://${keyPath}";
      canmount = "noauto";
    };

    configEncryptionProps =
      optionalAttrs cfg.configEncryption.enable (mkEncryptionProps configKeyPath);
    dataEncryptionProps =
      optionalAttrs cfg.dataEncryption.enable (mkEncryptionProps dataKeyPath);

    # dataDir conventionally nests inside baseDir's mountpoint. When both are
    # encrypted, their unlock units need explicit ordering: mounting the child
    # first would let the later parent mount shadow it, leaving the container
    # an empty data dir.
    dataDirNested = hasPrefix "${cfg.baseDir}/" cfg.dataDir;
  in {
    modules.linux.oci._managedPaths = {
      # Encryption properties go first so user-supplied *Properties win on conflict.
      ${cfg.baseDir}.properties = configEncryptionProps // cfg.configProperties;
      ${cfg.dataDir}.properties = dataEncryptionProps // cfg.dataProperties;
    };

    modules.services.zfs.encryptedDatasets =
      optionalAttrs cfg.configEncryption.enable {
        sftpgo-config = {
          dataset = removePrefix "/" cfg.baseDir;
          keyFile = configKeyPath;
          consumers =
            ["podman-sftpgo.service"]
            ++ optional (cfg.dataEncryption.enable && dataDirNested)
            "zfs-load-key-sftpgo-data.service";
        };
      }
      // optionalAttrs cfg.dataEncryption.enable {
        sftpgo-data = {
          dataset = removePrefix "/" cfg.dataDir;
          keyFile = dataKeyPath;
          consumers = ["podman-sftpgo.service"];
        };
      };

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "sftpgo/admin-password" = {};
      }
      // optionalAttrs cfg.oidc.enable {
        "sftpgo/oidc-client-id" = {};
        "sftpgo/oidc-client-secret" = {};
      }
      // optionalAttrs cfg.configEncryption.enable {
        # Opaque binary — `keyformat = raw` means ZFS expects exactly 32
        # bytes, so any text-mode round-trip would corrupt it.
        "sftpgo/config-zfs-key" = {
          format = "binary";
          sopsFile = cfg.configEncryption.keyFile;
        };
      }
      // optionalAttrs cfg.dataEncryption.enable {
        "sftpgo/data-zfs-key" = {
          format = "binary";
          sopsFile = cfg.dataEncryption.keyFile;
        };
      };

    sops.templates."sftpgo-env".content =
      ''
        SFTPGO_DEFAULT_ADMIN_PASSWORD=${config.sops.placeholder."sftpgo/admin-password"}
      ''
      + optionalString cfg.oidc.enable ''
        SFTPGO_HTTPD__BINDINGS__0__OIDC__CLIENT_ID=${config.sops.placeholder."sftpgo/oidc-client-id"}
        SFTPGO_HTTPD__BINDINGS__0__OIDC__CLIENT_SECRET=${config.sops.placeholder."sftpgo/oidc-client-secret"}
      '';

    virtualisation.oci-containers.containers.sftpgo = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment =
        {
          "TZ" = cfg.timezone;
          "SFTPGO_HTTPD__BINDINGS__0__PORT" = toString cfg.webPort;
          "SFTPGO_HTTPD__BINDINGS__0__ADDRESS" = "0.0.0.0";
          "SFTPGO_WEBDAVD__BINDINGS__0__PORT" = toString cfg.davPort;
          "SFTPGO_WEBDAVD__BINDINGS__0__ADDRESS" = "0.0.0.0";
          # proxy_mode stays 0 (that's the binary HAProxy PROXY protocol,
          # not HTTP header trust); header trust is governed solely by
          # proxy_allowed + the client IP header.
          "SFTPGO_WEBDAVD__BINDINGS__0__PROXY_ALLOWED" = concatStringsSep "," cfg.trustedProxies;
          "SFTPGO_WEBDAVD__BINDINGS__0__CLIENT_IP_PROXY_HEADER" = "X-Forwarded-For";
          "SFTPGO_SFTPD__BINDINGS__0__PORT" = toString cfg.sftpPort;
          "SFTPGO_DATA_PROVIDER__CREATE_DEFAULT_ADMIN" = "1";
          "SFTPGO_DEFAULT_ADMIN_USERNAME" = cfg.adminUsername;
          "SFTPGO_DATA_PROVIDER__USERS_BASE_DIR" = "/srv/sftpgo/data";
        }
        // optionalAttrs cfg.defender.enable {
          "SFTPGO_COMMON__DEFENDER__ENABLED" = "1";
        }
        // optionalAttrs cfg.oidc.enable {
          "SFTPGO_HTTPD__BINDINGS__0__OIDC__CONFIG_URL" = cfg.oidc.configUrl;
          "SFTPGO_HTTPD__BINDINGS__0__OIDC__REDIRECT_BASE_URL" = cfg.oidc.redirectBaseUrl;
          "SFTPGO_HTTPD__BINDINGS__0__OIDC__USERNAME_FIELD" = cfg.oidc.usernameField;
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."sftpgo-env".path];
      volumes =
        [
          "${cfg.baseDir}:/var/lib/sftpgo"
          "${cfg.dataDir}:/srv/sftpgo/data"
        ]
        ++ cfg.extraVolumes;
      ports =
        [
          "${toString cfg.webPort}:${toString cfg.webPort}"
          "${toString cfg.davPort}:${toString cfg.davPort}"
        ]
        ++ optional (cfg.sftpPort != 0) "${toString cfg.sftpPort}:${toString cfg.sftpPort}";
      extraOptions =
        [
          "--network-alias=sftpgo"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "sftpgo";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-sftpgo" = mkMerge [
      (ociLib.mkServiceConfig {
        inherit (cfg) networks;
        sopsTemplates = ["sftpgo-env"];
      })
      {
        # Fresh dataset mountpoints are root:root and the image has no
        # self-chown logic, so fix ownership of the dirs the container
        # writes. `install -d` is idempotent; leading `+` runs as root.
        serviceConfig.ExecStartPre = map (
          d: "+${pkgs.coreutils}/bin/install -d -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} ${d}"
        ) [cfg.baseDir cfg.dataDir];
      }
    ];
  });
}
