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
          ${optionalString (netCfg.driver != null) "--driver=${netCfg.driver}"} \
          ${optionalString (netCfg.subnet != null) "--subnet=${netCfg.subnet}"} \
          ${optionalString (netCfg.gateway != null) "--gateway=${netCfg.gateway}"}
    '';
    partOf = ["${rootTargetName}.target"];
    wantedBy = ["${rootTargetName}.target"];
  };

  # Generate systemd service for a named volume
  mkVolumeService = name: volCfg: {
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
    };
  };

  # Volume option type
  volumeOpts = {name, ...}: {
    options = {
      enable = mkEnableOption "volume ${name}";
    };
  };

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
    # mounted via volumes or environmentFiles). The pre-rendered content
    # is hashed into restartTriggers so a switch-to-configuration
    # restarts the container when the template's source changes — without
    # this the container holds stale config across deploys because its
    # mount path is stable while only the file content changed.
    sopsTemplates ? [],
  }: let
    zfsDeps = optional zfsCfg.enable "zfs-manage-datasets.service";
    templateTriggers =
      map (n: config.sops.templates.${n}.content) sopsTemplates;
  in {
    restartTriggers = templateTriggers;
    serviceConfig = {
      Restart = mkOverride 90 "always";
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
    ./cadvisor.nix
    ./calibre.nix
    ./calibre-web-auto.nix
    ./filebrowser.nix
    ./flaresolverr.nix
    ./forgejo.nix
    ./forgejo-runner.nix
    ./gluetun.nix
    ./grafana.nix
    ./immich.nix
    ./immich-ml.nix
    ./jellyfin.nix
    ./lidarr.nix
    ./linkding.nix
    ./loki.nix
    ./miniflux.nix
    ./navidrome.nix
    ./newt.nix
    ./nzbget.nix
    ./open-webui.nix
    ./netbird.nix
    ./pangolin.nix
    ./paperless.nix
    ./pihole.nix
    ./plex.nix
    ./pocket-id.nix
    ./podman-exporter.nix
    ./prometheus.nix
    ./prowlarr.nix
    ./radarr.nix
    ./recyclarr.nix
    ./requestrr.nix
    ./slskd.nix
    ./sonarr.nix
    ./soularr.nix
    ./sure.nix
    ./syncthing.nix
    ./tautulli.nix
    ./transmission.nix
    ./vikunja.nix
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
  # into every container regardless of where it's defined.
  options.virtualisation.oci-containers.containers = mkOption {
    type = types.attrsOf (types.submodule {
      config.extraOptions = mkAfter ["--no-healthcheck"];
    });
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

    assertions = mkIf zfsCfg.enable managedPathAssertions;

    virtualisation.podman = {
      enable = true;
      autoPrune.enable = true;
      dockerCompat = mkDefault true;
    };

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
