{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci;
  hostname = config.networking.hostName;

  # Naming conventions (matches compose2nix output)
  networkServiceName = name: "podman-network-${hostname}_${name}";
  volumeServiceName = name: "podman-volume-${hostname}_${name}";
  rootTargetName = "podman-compose-${hostname}-root";

  # Generate systemd service for a network
  mkNetworkService = name: netCfg: {
    path = [pkgs.podman];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStop = "${pkgs.podman}/bin/podman network rm -f ${hostname}_${name}";
    };
    script = ''
      ${pkgs.podman}/bin/podman network inspect ${hostname}_${name} || \
        ${pkgs.podman}/bin/podman network create ${hostname}_${name} \
          ${optionalString (netCfg.driver == null || netCfg.driver == "bridge") "--opt=isolate=${netCfg.isolate}"} \
          ${optionalString (netCfg.driver != null) "--driver=${netCfg.driver}"} \
          ${optionalString (netCfg.subnet != null) "--subnet=${netCfg.subnet}"} \
          ${optionalString (netCfg.gateway != null) "--gateway=${netCfg.gateway}"}
    '';
    partOf = ["${rootTargetName}.target"];
    wantedBy = ["${rootTargetName}.target"];
  };

  # Generate systemd service for a named volume
  mkVolumeService = name: _volCfg: {
    path = [pkgs.podman];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      ${pkgs.podman}/bin/podman volume inspect ${hostname}_${name} || \
        ${pkgs.podman}/bin/podman volume create ${hostname}_${name}
    '';
    partOf = ["${rootTargetName}.target"];
    wantedBy = ["${rootTargetName}.target"];
  };

  # Network option type
  networkOpts = {name, ...}: {
    options = {
      enable = mkEnableOption "network ${name}";
      driver = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Network driver (bridge, macvlan, etc.)";
      };
      subnet = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Subnet in CIDR notation";
      };
      gateway = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Gateway IP address";
      };
      isolate = mkOption {
        type = types.enum ["false" "true" "strict"];
        default = "false";
        description = "netavark inter-network isolation (bridge driver only). netavark >= 2.0 defaults to strict, which also drops container traffic hairpinning through host-published ports on other networks; the module default restores the pre-2.0 behavior its services assume.";
      };
    };
  };

  # Volume option type
  volumeOpts = {name, ...}: {
    options = {
      enable = mkEnableOption "volume ${name}";
    };
  };

  # The host's own trust bundle, as built by security.pki (so every distrust
  # applied there is already in it), laid out as the /etc/ssl/certs directory
  # images expect. The no-trust-rules variant is plain PEM: Go, Python and Ruby
  # parse only plain CERTIFICATE blocks, and the rejected-root entries in the
  # OpenSSL trust-rules bundle mean nothing to them.
  # ca-bundle.crt is the name Fedora-family and Nix-built images point at.
  hostTrustDir = pkgs.runCommand "oci-host-ca-certificates" {} ''
    mkdir $out
    cp ${dirOf config.security.pki.caBundle}/ca-no-trust-rules-bundle.crt $out/ca-certificates.crt
    ln -s ca-certificates.crt $out/ca-bundle.crt
  '';
  hostTrustBundle = "/etc/ssl/certs/ca-certificates.crt";

  # Filter to enabled items
  enabledNetworks = filterAttrs (_: v: v.enable) cfg.networks;
  enabledVolumes = filterAttrs (_: v: v.enable) cfg.volumes;

  # ZFS dataset generation from collected paths.
  # Convention: mountpoints live under /${pool}/... (e.g. pool=data and
  # path=/data/apps/foo). The dataset name is the path with its leading
  # slash stripped, so /data/apps/foo → data/apps/foo. Registering a
  # path outside the pool's namespace is unsupported — the formula
  # can't invent a canonical dataset name for arbitrary host paths.
  zfsCfg = cfg.zfs;
  managedDatasets =
    mapAttrs' (
      path: pathCfg:
        nameValuePair (removePrefix "/" path) {
          properties = {mountpoint = path;} // zfsCfg.properties // pathCfg.properties;
        }
    )
    cfg._managedPaths;
  managedPathAssertions =
    mapAttrsToList (path: _: {
      assertion = hasPrefix "/${zfsCfg.pool}/" path;
      message = "modules.linux.oci._managedPaths: path '${path}' is outside pool '${zfsCfg.pool}'. Register paths under /${zfsCfg.pool}/... or extend the helper to accept an explicit dataset name.";
    })
    cfg._managedPaths;

  # Helper to generate systemd service config for a container. Defined in
  # the outer let block so mkArrService (below, in the lib attrset) can
  # reuse it without needing `rec` on the lib.
  mkServiceConfig' = {
    networks ? ["default"],
    volumes ? [],
    extraAfter ? [],
    extraRequires ? [],
    # Names of sops.templates the container reads at startup (typically
    # mounted via volumes or environmentFiles). What gets hashed into
    # restartTriggers is the template's *pre-render* content — placeholder
    # tokens, not decrypted values — so a switch restarts the container
    # when the template text or its placeholder set changes, but NOT when
    # a secret's value rotates: the rendered file updates on disk while
    # the running container keeps its stale copy. Secret rotations need a
    # manual `systemctl restart podman-<svc>` after the deploy.
    sopsTemplates ? [],
  }: let
    zfsDeps = optional zfsCfg.enable "zfs-manage-datasets.service";
    templateTriggers =
      map (n: config.sops.templates.${n}.content) sopsTemplates;
  in {
    restartTriggers = templateTriggers;
    # Keep retrying through transient startup failures instead of latching to
    # systemd's default start-limit (5 starts / 10s) and giving up. Motivating
    # case: on hosts that resolve DNS through their own Pi-hole container, a
    # deploy that bumps a service's pinned image digest forces a registry pull
    # at the same moment Pi-hole is restarting — the pull fails with "no such
    # host", burns all 5 starts in ~2s, and the unit stays dead until a human
    # intervenes even though DNS recovers seconds later. RestartSec spaces
    # retries past that gap; disabling the start-limit lets Restart=always mean
    # what it says. Genuine liveness is watched via the Prometheus blackbox
    # path, not systemd's failed state.
    startLimitIntervalSec = mkOverride 90 0;
    serviceConfig = {
      Restart = mkOverride 90 "always";
      RestartSec = mkOverride 90 10;
    };
    after =
      zfsDeps
      ++ (map (n: "${networkServiceName n}.service") networks)
      ++ (map (v: "${volumeServiceName v}.service") volumes)
      ++ extraAfter;
    requires =
      zfsDeps
      ++ (map (n: "${networkServiceName n}.service") networks)
      ++ (map (v: "${volumeServiceName v}.service") volumes)
      ++ extraRequires;
    partOf = ["${rootTargetName}.target"];
    wantedBy = ["${rootTargetName}.target"];
  };
in {
  imports = [
    ./bazarr.nix
    ./caddy.nix
    ./calibre-web-auto.nix
    ./calibre.nix
    ./crowdsec.nix
    ./ebay-mcp.nix
    ./filebrowser.nix
    ./flaresolverr.nix
    ./forgejo-runner.nix
    ./forgejo.nix
    ./gluetun.nix
    ./grafana.nix
    ./immich-ml.nix
    ./immich.nix
    ./jellyfin.nix
    ./job-ops.nix
    ./kept.nix
    ./kitchenowl.nix
    ./lidarr.nix
    ./linkding.nix
    ./loki.nix
    ./miniflux.nix
    ./navidrome.nix
    ./netbird.nix
    ./newt.nix
    ./nzbget.nix
    ./open-webui.nix
    ./pangolin.nix
    ./paperless.nix
    ./pihole.nix
    ./plex.nix
    ./pocket-id.nix
    ./podman-exporter.nix
    ./prometheus.nix
    ./prowlarr.nix
    ./radarr.nix
    ./radicale.nix
    ./recyclarr.nix
    ./registry-auth.nix
    ./requestrr.nix
    ./seerr.nix
    ./sftpgo.nix
    ./shelfmark.nix
    ./slskd.nix
    ./sonarr.nix
    ./soularr.nix
    ./sure.nix
    ./syncthing.nix
    ./tautulli.nix
    ./trading-bot.nix
    ./transmission.nix
    ./tumblr-alt-text-bot.nix
    ./vikunja.nix
    ./wolf-den.nix
    ./wolf.nix
  ];

  options.modules.linux.oci = {
    enable = mkEnableOption "OCI container infrastructure";

    networks = mkOption {
      type = types.attrsOf (types.submodule networkOpts);
      default = {};
      description = "Podman networks to create";
    };

    volumes = mkOption {
      type = types.attrsOf (types.submodule volumeOpts);
      default = {};
      description = "Podman named volumes to create";
    };

    zfs = {
      enable = mkEnableOption "ZFS dataset management for OCI service directories";
      pool = mkOption {
        type = types.str;
        description = "ZFS pool name to create datasets under.";
        example = "zroot";
      };
      properties = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Default ZFS properties applied to all generated datasets.";
        example = {compression = "lz4";};
      };
    };

    _managedPaths = mkOption {
      type = types.attrsOf (types.submodule {
        options.properties = mkOption {
          type = types.attrsOf types.str;
          default = {};
          description = "Per-path ZFS properties (overrides global zfs.properties).";
        };
      });
      internal = true;
      default = {};
      description = "Host paths collected from OCI services to be managed as ZFS datasets.";
    };

    _gluetunPorts = mkOption {
      type = types.listOf types.str;
      internal = true;
      default = [];
      description = ''
        Aggregated host port mappings contributed by services that share the
        gluetun container's network namespace. The gluetun service module
        publishes these on the host since downstream containers can't bind
        host ports themselves once joined via --network=container:gluetun.
      '';
    };

    _gluetunVpnInputPortSecrets = mkOption {
      type = types.listOf types.str;
      internal = true;
      default = [];
      description = ''
        Sops secret names, each holding a port that gluetun must accept
        inbound on its VPN interface (FIREWALL_VPN_INPUT_PORTS). Contributed
        by services that listen for peers through the tunnel. The ports are
        secret because a provider-forwarded port identifies the account.
      '';
    };

    hostTrustStore = mkOption {
      type = types.attrsOf (types.submodule {
        options.node = mkOption {
          type = types.bool;
          default = false;
          description = ''
            The container runs Node.js, which verifies against a root list
            compiled into its binary and ignores the system store. Sets
            NODE_OPTIONS=--use-openssl-ca so it reads the mounted bundle
            instead; this replaces any NODE_OPTIONS the image defines.
          '';
        };
        options.bundlePaths = mkOption {
          type = types.listOf types.str;
          default = [];
          example = ["/usr/lib/plexmediaserver/Resources/cacert.pem"];
          description = ''
            Further in-container files to replace with the host bundle, for
            apps that read a CA file of their own and ignore the environment.
          '';
        };
      });
      default = {};
      example = literalExpression ''
        {
          newt = {};
          immich_server.node = true;
        }
      '';
      description = ''
        Containers, by name, that verify TLS against the host's CA bundle
        instead of the one frozen into their image. The bundle is mounted over
        /etc/ssl/certs and named by SSL_CERT_FILE, SSL_CERT_DIR and
        REQUESTS_CA_BUNDLE, so a CA distrusted through security.pki reaches
        the container on the next deploy rather than whenever upstream
        rebuilds the image. Anything else in the image's /etc/ssl/certs, such
        as Java's cacerts or extra CAs added at build time, is hidden.
      '';
    };

    lib = mkOption {
      type = types.attrs;
      internal = true;
      description = "Helper functions and values for OCI service modules.";
    };
  };

  # Disable image-level HEALTHCHECKs across the board for every container
  # registered via virtualisation.oci-containers. Podman spawns a transient
  # `systemd-run` timer + service per container with a HEALTHCHECK, and the
  # first firing races container start-up: the check command exits 1 (app
  # not ready), the transient unit goes to `failed`, and
  # switch-to-configuration treats that as activation failure — which makes
  # deploy-rs roll back even though every container actually came up fine.
  # `--health-start-period` only suppresses podman's `health_status` flag,
  # not the CLI's exit code, so it does not fix this. systemd's
  # `Restart=always` (set in mkServiceConfig) handles the actual "process
  # died, restart it" behaviour we used the healthcheck for. Real
  # liveness/readiness lives in the Prometheus blackbox path instead.
  #
  # We extend the submodule type rather than reading the merged containers
  # attrset (which causes infinite recursion) — this injects extraOptions
  # into every container regardless of where it's defined. The same hook
  # applies hostTrustStore to the containers it names.
  options.virtualisation.oci-containers.containers = mkOption {
    type = types.attrsOf (types.submodule ({name, ...}: {
      config = mkMerge [
        {extraOptions = mkAfter ["--no-healthcheck"];}
        (mkIf (cfg.hostTrustStore ? ${name}) {
          volumes =
            ["${hostTrustDir}:/etc/ssl/certs:ro"]
            ++ map (p: "${hostTrustDir}/ca-certificates.crt:${p}:ro") cfg.hostTrustStore.${name}.bundlePaths;
          environment =
            {
              SSL_CERT_FILE = hostTrustBundle;
              # Go also reads every file in its default certificate
              # directories, /etc/pki/tls/certs among them, unless this
              # narrows the search.
              SSL_CERT_DIR = "/etc/ssl/certs";
              # requests verifies against certifi's own copy of the roots
              # unless pointed elsewhere.
              REQUESTS_CA_BUNDLE = hostTrustBundle;
            }
            // optionalAttrs cfg.hostTrustStore.${name}.node {
              NODE_OPTIONS = "--use-openssl-ca";
            };
        })
      ];
    }));
  };

  config = mkIf cfg.enable {
    # Exposed for service modules to reference
    modules.linux.oci.lib = {
      inherit hostname rootTargetName networkServiceName volumeServiceName;

      # Full network name as it appears in podman
      networkName = name: "${hostname}_${name}";

      # Full volume name as it appears in podman
      volumeName = name: "${hostname}_${name}";

      # Helper to generate systemd service config for a container
      mkServiceConfig = mkServiceConfig';

      # Helper for linuxserver.io-style "*arr" services (radarr, sonarr,
      # transmission, etc.). Returns separate pieces that the caller composes
      # into its own attrset literal under `mkIf cfg.enable {...}` — this
      # keeps the function call lazy across the module-system pushdown phase
      # and avoids cycles when reading config.modules.linux.oci.lib.
      #
      # When `useGluetun` is true, the container joins the gluetun container's
      # network namespace and its host port mappings are forwarded onto
      # gluetun via _gluetunPorts.
      mkArrService = {
        name,
        image,
        baseDir,
        containerConfigPath ? "/config",
        configProperties ? {},
        mediaMounts ? [],
        useGluetun ? false,
        gluetunContainer ? "gluetun",
        ports ? [],
        gluetunPorts ? [],
        networks ? ["default"],
        extraEnv ? {},
        extraOptions ? [],
        environmentFiles ? [],
        dependsOn ? [],
        user ? {
          uid = 1000;
          gid = 100;
        },
        timezone ? "America/New_York",
        capAdd ? [],
        sopsTemplates ? [],
      }: let
        netOpts =
          if useGluetun
          then ["--network=container:${gluetunContainer}"]
          else
            ["--network-alias=${name}"]
            ++ (map (n: "--network=${hostname}_${n}") networks);
        gluetunDeps = optional useGluetun "podman-${gluetunContainer}.service";
      in {
        container = {
          inherit image dependsOn environmentFiles;
          environment =
            {
              "PUID" = toString user.uid;
              "PGID" = toString user.gid;
              "TZ" = timezone;
            }
            // extraEnv;
          volumes =
            ["${baseDir}:${containerConfigPath}:rw"]
            ++ mediaMounts;
          ports = optionals (!useGluetun) ports;
          extraOptions =
            netOpts
            ++ (map (c: "--cap-add=${c}") capAdd)
            ++ extraOptions;
          log-driver = "journald";
        };

        serviceConfig = mkServiceConfig' {
          networks =
            if useGluetun
            then []
            else networks;
          extraAfter = gluetunDeps;
          extraRequires = gluetunDeps;
          inherit sopsTemplates;
        };

        managedPaths = {
          ${baseDir} = {properties = configProperties;};
        };

        gluetunPorts = optionals useGluetun gluetunPorts;

        networks =
          if useGluetun
          then {}
          else listToAttrs (map (n: nameValuePair n {enable = true;}) networks);
      };
    };
    # Generate ZFS datasets from collected service paths
    modules.services.zfs.datasets = mkIf (zfsCfg.enable && cfg._managedPaths != {}) managedDatasets;

    # Plain-filesystem fallback: when ZFS isn't managing these paths, ensure
    # they exist as directories so podman bind-mounts don't fail with
    # `statfs ... no such file or directory` on first start.
    systemd.tmpfiles.rules = mkIf (!zfsCfg.enable) (
      mapAttrsToList (path: _: "d ${path} 0755 root root - -") cfg._managedPaths
    );

    assertions =
      optionals zfsCfg.enable managedPathAssertions
      ++ mapAttrsToList (name: _: {
        assertion = config.virtualisation.oci-containers.containers ? ${name};
        message = "modules.linux.oci.hostTrustStore.${name}: no container named '${name}' is defined.";
      })
      cfg.hostTrustStore;

    virtualisation.podman = {
      enable = true;
      # A bare `podman system prune -f` only reclaims *dangling* images — those
      # with neither tags nor repo digests. Services here pin images by digest
      # (oci-images.json), so every superseded image keeps a RepoDigest and is
      # never dangling: the default prune reclaims nothing and each digest bump
      # leaks a whole image. `--all` also drops images with no running container.
      # Consequence: a rolled-back generation must re-pull its pinned digests.
      autoPrune = {
        enable = true;
        flags = ["--all"];
      };
      dockerCompat = mkDefault true;
    };

    # The registries.conf v2 rewrite in nixpkgs no longer defines
    # unqualified-search-registries by default (its [[registry]] blocks alone
    # don't enable short-name resolution), which breaks every short-name image
    # reference like "valkey/valkey" at pull time.
    virtualisation.containers.registries.settings.unqualified-search-registries = ["docker.io"];

    # Allow DNS from container interfaces
    networking.firewall.interfaces = let
      matchAll =
        if !config.networking.nftables.enable
        then "podman+"
        else "podman*";
    in {
      "${matchAll}".allowedUDPPorts = [53];
    };

    virtualisation.oci-containers.backend = "podman";

    systemd = {
      # Create network services
      services =
        (mapAttrs'
          (name: netCfg: nameValuePair (networkServiceName name) (mkNetworkService name netCfg))
          enabledNetworks)
        // (mapAttrs'
          (name: volCfg: nameValuePair (volumeServiceName name) (mkVolumeService name volCfg))
          enabledVolumes);

      # Root target for all OCI services
      targets."${rootTargetName}" = {
        unitConfig = {
          Description = "Root target for ${hostname} OCI containers";
        };
        wantedBy = ["multi-user.target"];
      };
    };
  };
}
