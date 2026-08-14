{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.gluetun;
  ociLib = config.modules.linux.oci.lib;
  ociCfg = config.modules.linux.oci;
  imageLib = import ./lib.nix {inherit lib;};
in {
  options.modules.linux.oci.services.gluetun = {
    enable = mkEnableOption "Gluetun VPN gateway";

    image = imageLib.mkImageOptions {
      repository = "qmcgaw/gluetun";
      version = "latest";
    };

    baseDir = mkOption {
      description = "Base directory for gluetun state (mounted at /gluetun).";
      type = types.str;
      example = "/data/apps/gluetun";
    };

    vpnProvider = mkOption {
      description = "VPN service provider (gluetun VPN_SERVICE_PROVIDER).";
      type = types.str;
      default = "airvpn";
      example = "mullvad";
    };

    vpnType = mkOption {
      description = "VPN protocol (gluetun VPN_TYPE).";
      type = types.enum ["wireguard" "openvpn"];
      default = "wireguard";
    };

    ownedOnly = mkOption {
      description = ''
        Restrict to provider-owned servers only. Mullvad-specific; only
        emitted as OWNED_ONLY when vpnProvider is "mullvad".
      '';
      type = types.bool;
      default = true;
    };

    usePresharedKey = mkOption {
      description = ''
        Whether the provider requires a WireGuard preshared key. AirVPN's
        config generator always issues one; Mullvad does not use them. When
        true, the gluetun/wireguard-preshared-key sops secret must exist.
      '';
      type = types.bool;
      default = cfg.vpnProvider == "airvpn";
    };

    serverCountries = mkOption {
      description = "Restrict server selection to these countries (SERVER_COUNTRIES).";
      type = types.listOf types.str;
      default = [];
      example = ["Netherlands" "Switzerland"];
    };

    serverCities = mkOption {
      description = "Restrict server selection to these cities (SERVER_CITIES).";
      type = types.listOf types.str;
      default = [];
    };

    serverNames = mkOption {
      description = "Restrict server selection to these server names (SERVER_NAMES).";
      type = types.listOf types.str;
      default = [];
    };

    wireguardEndpointPort = mkOption {
      description = ''
        Override the UDP port gluetun connects to (WIREGUARD_ENDPOINT_PORT),
        keeping provider server selection. null leaves gluetun's per-provider
        default. The value must be one the provider accepts or gluetun aborts
        at startup; AirVPN allows only 1637 (default) or 47107.
      '';
      type = types.nullOr types.port;
      default = null;
      example = 47107;
    };

    timezone = mkOption {
      description = "Timezone for the container.";
      type = types.str;
      default = "America/New_York";
    };

    networks = mkOption {
      description = "Networks the gluetun container itself joins.";
      type = types.listOf types.str;
      default = ["default"];
    };

    extraPorts = mkOption {
      description = ''
        Additional host port mappings to publish on gluetun, beyond those
        contributed by downstream services via _gluetunPorts. Use for
        gluetun's own services (HTTP proxy, shadowsocks).
      '';
      type = types.listOf types.str;
      default = [];
      example = ["8888:8888/tcp" "8388:8388/tcp" "8388:8388/udp"];
    };

    extraEnv = mkOption {
      description = "Additional environment variables.";
      type = types.attrsOf types.str;
      default = {};
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    sops.secrets =
      {
        "gluetun/wireguard-private-key" = {};
        # Assigned VPN IP is identifying material — kept in sops so it
        # never appears in the nix store.
        "gluetun/wireguard-addresses" = {};
      }
      // optionalAttrs cfg.usePresharedKey {
        "gluetun/wireguard-preshared-key" = {};
      };

    sops.templates."gluetun-env".content =
      ''
        WIREGUARD_PRIVATE_KEY=${config.sops.placeholder."gluetun/wireguard-private-key"}
        WIREGUARD_ADDRESSES=${config.sops.placeholder."gluetun/wireguard-addresses"}
      ''
      + optionalString cfg.usePresharedKey ''
        WIREGUARD_PRESHARED_KEY=${config.sops.placeholder."gluetun/wireguard-preshared-key"}
      '';

    virtualisation.oci-containers.containers."gluetun" = {
      image = imageLib.renderImage cfg.image;
      environment =
        {
          "VPN_SERVICE_PROVIDER" = cfg.vpnProvider;
          "VPN_TYPE" = cfg.vpnType;
          "TZ" = cfg.timezone;
        }
        // optionalAttrs (cfg.vpnProvider == "mullvad") {
          "OWNED_ONLY" =
            if cfg.ownedOnly
            then "yes"
            else "no";
        }
        // optionalAttrs (cfg.serverCountries != []) {
          "SERVER_COUNTRIES" = concatStringsSep "," cfg.serverCountries;
        }
        // optionalAttrs (cfg.serverCities != []) {
          "SERVER_CITIES" = concatStringsSep "," cfg.serverCities;
        }
        // optionalAttrs (cfg.serverNames != []) {
          "SERVER_NAMES" = concatStringsSep "," cfg.serverNames;
        }
        // optionalAttrs (cfg.wireguardEndpointPort != null) {
          "WIREGUARD_ENDPOINT_PORT" = toString cfg.wireguardEndpointPort;
        }
        // cfg.extraEnv;
      environmentFiles = [config.sops.templates."gluetun-env".path];
      volumes = [
        "${cfg.baseDir}:/gluetun"
      ];
      ports = cfg.extraPorts ++ ociCfg._gluetunPorts;
      extraOptions =
        [
          "--cap-add=NET_ADMIN"
          "--device=/dev/net/tun:/dev/net/tun"
          "--network-alias=gluetun"
        ]
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks)
        ++ imageLib.mkImageLabels {
          module = "gluetun";
          inherit (cfg) image;
        };
      log-driver = "journald";
    };

    systemd.services."podman-gluetun" = ociLib.mkServiceConfig {
      inherit (cfg) networks;
      sopsTemplates = ["gluetun-env"];
    };
  };
}
