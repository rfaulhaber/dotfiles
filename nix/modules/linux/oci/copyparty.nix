{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.copyparty;
  ociLib = config.modules.linux.oci.lib;
  imageLib = import ./lib.nix {inherit lib;};

  # Volumes land under /w inside the container, mirroring upstream's docker
  # examples ("/" -> /w, "/backups" -> /w/backups). The image creates /cfg,
  # /w and /state world-writable at build time and has no PUID/PGID logic —
  # it runs as whatever --user says.
  containerPathFor = urlPath: vol:
    if vol.containerPath != null
    then vol.containerPath
    else if urlPath == "/"
    then "/w"
    else "/w${urlPath}";

  volumeOpts = {
    options = {
      hostPath = mkOption {
        description = "Host directory backing this volume.";
        type = types.str;
      };

      containerPath = mkOption {
        description = "Mount point inside the container. Defaults to /w plus the volume's URL path.";
        type = types.nullOr types.str;
        default = null;
      };

      zfsManaged = mkOption {
        description = ''
          Register hostPath as a managed ZFS dataset. Use for volume roots
          owned by this service; leave false for shared datasets declared
          elsewhere (those must exist and be writable by the container
          user before the container starts).
        '';
        type = types.bool;
        default = false;
      };

      properties = mkOption {
        description = "ZFS properties for the managed dataset (zfsManaged only).";
        type = types.attrsOf types.str;
        default = {};
      };

      accs = mkOption {
        description = ''
          Permission-string to account-list mapping, rendered into the
          volume's accs: block. Permission letters: r read, w write,
          m move, d delete, "." see dotfiles, g get, a admin, A all.
          WebDAV clients that re-upload the same filename (backup apps)
          need d plus the daw volflag, or uploads become numbered
          sidecar copies. Values may be account names, "*", or @group.
        '';
        type = types.attrsOf (types.listOf types.str);
        default = {};
        example = {"rwmda." = ["ryan"];};
      };

      flags = mkOption {
        description = ''Volflags, one per entry, rendered verbatim (e.g. "daw", "nohash: \.iso$").'';
        type = types.listOf types.str;
        default = [];
      };
    };
  };
in {
  options.modules.linux.oci.services.copyparty = {
    enable = mkEnableOption "copyparty file server (web UI + WebDAV)";

    image = imageLib.mkImageOptions {
      # Docker Hub's copyparty/ac versioned tags are stale (stop at 1.9.x);
      # GHCR carries current semver tags. "ac" is the upstream-recommended
      # edition (Pillow + FFmpeg for thumbnails/transcodes).
      repository = "ghcr.io/9001/copyparty-ac";
      version = "1.20.21";
    };

    baseDir = mkOption {
      description = ''
        State directory, mounted at /cfg. Holds the rendered config (any
        *.conf in /cfg is auto-included by the image) and the centralized
        .hist databases (up2k index, thumbnails — hence the SQLite-tuned
        recordsize default).
      '';
      type = types.str;
      example = "/data/apps/copyparty";
    };

    webPort = mkOption {
      description = ''
        Port for HTTP (web UI + WebDAV) — used for both the host publish
        and the container's internal listen, which is rendered into the
        config's `p:` setting.
      '';
      type = types.port;
      default = 3923;
    };

    accounts = mkOption {
      description = ''
        Account names. Each pulls its password from the sops secret
        "copyparty/accounts/<name>". Passwords must be unique across
        accounts: copyparty's basic auth is password-keyed (the username
        field is decorative), so a duplicate password is ambiguous.
      '';
      type = types.listOf types.str;
      default = [];
    };

    volumes = mkOption {
      description = "Volumes keyed by URL path (\"/\" is the webroot).";
      type = types.attrsOf (types.submodule volumeOpts);
      default = {};
    };

    idp = {
      enable = mkOption {
        description = ''
          Trust a reverse proxy to assert the authenticated username via
          header. Requests without a trusted IdP header (WebDAV clients,
          direct LAN access) fall back to the password accounts.
        '';
        type = types.bool;
        default = false;
      };

      userHeader = mkOption {
        description = "Header carrying the authenticated username (Pangolin's badger sends Remote-User).";
        type = types.str;
        default = "remote-user";
      };

      groupHeader = mkOption {
        description = "Optional header carrying group membership (badger sends Remote-Role).";
        type = types.nullOr types.str;
        default = null;
      };
    };

    xffSrc = mkOption {
      description = ''
        Peers trusted to assert X-Forwarded-For and the IdP headers.
        Failing this check is silent: the request just falls back to
        password auth. Keep it to the podman networks (where newt
        delivers tunneled traffic) — a LAN client hitting the published
        port arrives with its real source address and stays untrusted.
      '';
      type = types.listOf types.str;
      default = ["10.89.0.0/16"];
    };

    rproxy = mkOption {
      description = ''
        Which X-Forwarded-For entry is the client IP (positive = from the
        left, negative = from the right; -1 = the entry appended by the
        nearest trusted proxy). xff-src alone is inert: without rproxy
        set, copyparty falls back to the raw socket IP, so its
        brute-force bans would key on the reverse proxy's address and a
        remote attacker could get the whole tunnel banned.
      '';
      type = types.int;
      default = -1;
    };

    extraGlobalSettings = mkOption {
      description = ''Extra [global] config lines, rendered verbatim (e.g. "shr: /share").'';
      type = types.listOf types.str;
      default = [];
    };

    user = {
      uid = mkOption {
        description = "UID the container runs as (via --user).";
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
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };

    configProperties = mkOption {
      description = "ZFS properties applied to the baseDir dataset. Defaults tuned for SQLite.";
      type = types.attrsOf types.str;
      default = {recordsize = "64K";};
    };
  };

  config = mkIf cfg.enable (let
    managedVolumes = filterAttrs (_: v: v.zfsManaged) cfg.volumes;

    renderVolume = urlPath: vol:
      [
        "[${urlPath}]"
        "  ${containerPathFor urlPath vol}"
        "  accs:"
      ]
      ++ mapAttrsToList (perm: users: "    ${perm}: ${concatStringsSep ", " users}") vol.accs
      ++ optionals (vol.flags != []) (["  flags:"] ++ map (f: "    ${f}") vol.flags)
      ++ [""];

    configContent = concatStringsSep "\n" (
      [
        "[global]"
        "  p: ${toString cfg.webPort}"
        # Always-on indexing: e2dsa builds the file index (search, dedup
        # detection), e2ts scans media tags. Both live in hist below.
        "  e2dsa"
        "  e2ts"
        "  hist: /cfg/hists/"
        "  xff-src: ${concatStringsSep ", " cfg.xffSrc}"
        "  rproxy: ${toString cfg.rproxy}"
      ]
      ++ optionals cfg.idp.enable (
        ["  idp-h-usr: ${cfg.idp.userHeader}"]
        ++ optional (cfg.idp.groupHeader != null) "  idp-h-grp: ${cfg.idp.groupHeader}"
      )
      ++ map (s: "  ${s}") cfg.extraGlobalSettings
      ++ [""]
      ++ optionals (cfg.accounts != []) (
        ["[accounts]"]
        ++ map (name: "  ${name}: ${config.sops.placeholder."copyparty/accounts/${name}"}") cfg.accounts
        ++ [""]
      )
      ++ concatLists (mapAttrsToList renderVolume cfg.volumes)
    );
  in {
    modules.linux.oci._managedPaths =
      {${cfg.baseDir}.properties = cfg.configProperties;}
      // mapAttrs' (_: v: nameValuePair v.hostPath {inherit (v) properties;}) managedVolumes;

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets = listToAttrs (
      map (name: nameValuePair "copyparty/accounts/${name}" {}) cfg.accounts
    );

    sops.templates."copyparty-config" = {
      content = configContent;
      # World-readable so the non-root container user can read it through
      # the bind mount. The file lives under /run/secrets/ which is itself
      # only traversable by members of the keys group + container runtimes
      # podman exposes.
      mode = "0444";
    };

    virtualisation.oci-containers.containers.copyparty = {
      image = imageLib.renderImage cfg.image;
      inherit (cfg) dependsOn;
      environment = {"TZ" = cfg.timezone;} // cfg.extraEnv;
      volumes =
        ["${cfg.baseDir}:/cfg"]
        ++ mapAttrsToList (urlPath: v: "${v.hostPath}:${containerPathFor urlPath v}") cfg.volumes
        # Shadow the rendered config on top of the baseDir mount so the
        # on-disk copy in baseDir can never drift from the nix-rendered one.
        ++ ["${config.sops.templates."copyparty-config".path}:/cfg/copyparty.conf:ro"];
      ports = ["${toString cfg.webPort}:${toString cfg.webPort}"];
      extraOptions =
        [
          "--network-alias=copyparty"
          "--user=${toString cfg.user.uid}:${toString cfg.user.gid}"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "copyparty";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-copyparty" = mkMerge [
      (ociLib.mkServiceConfig {
        inherit (cfg) networks;
        sopsTemplates = ["copyparty-config"];
      })
      {
        # Fresh dataset mountpoints are root:root and the image has no
        # self-chown logic, so fix ownership of the dirs the container
        # writes. `install -d` is idempotent; leading `+` runs as root.
        serviceConfig.ExecStartPre = map (
          d: "+${pkgs.coreutils}/bin/install -d -o ${toString cfg.user.uid} -g ${toString cfg.user.gid} ${d}"
        ) ([cfg.baseDir] ++ mapAttrsToList (_: v: v.hostPath) managedVolumes);
      }
    ];
  });
}
