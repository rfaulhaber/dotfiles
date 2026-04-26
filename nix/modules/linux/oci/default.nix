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

  # ZFS dataset generation from collected paths
  zfsCfg = cfg.zfs;
  managedDatasets = mapAttrs' (path: pathCfg:
    nameValuePair "${zfsCfg.pool}${path}" {
      properties = {mountpoint = path;} // zfsCfg.properties // pathCfg.properties;
    }
  ) cfg._managedPaths;
in {
  imports = [
    ./caddy.nix
    ./forgejo-runner.nix
    ./immich.nix
    ./immich-ml.nix
    ./jellyfin.nix
    ./miniflux.nix
    ./newt.nix
    ./open-webui.nix
    ./pihole.nix
    ./plex.nix
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

    lib = mkOption {
      type = types.attrs;
      internal = true;
      description = "Helper functions and values for OCI service modules.";
    };
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
      mkServiceConfig = {
        networks ? ["default"],
        volumes ? [],
        extraAfter ? [],
        extraRequires ? [],
      }: let
        zfsDeps = optional zfsCfg.enable "zfs-manage-datasets.service";
      in {
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
    };
    # Generate ZFS datasets from collected service paths
    modules.services.zfs.datasets = mkIf (zfsCfg.enable && cfg._managedPaths != {}) managedDatasets;

    virtualisation.podman = {
      enable = true;
      autoPrune.enable = true;
      dockerCompat = true;
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
