{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.linux.oci.services.gluetun;
  ociLib = config.modules.linux.oci.lib;
  ociCfg = config.modules.linux.oci;
in {
  options.modules.linux.oci.services.gluetun = {
    enable = mkEnableOption "Gluetun VPN gateway";

    image = mkOption {
      description = "Gluetun container image.";
      type = types.str;
      default = "qmcgaw/gluetun";
    };

    baseDir = mkOption {
      description = "Base directory for gluetun state (mounted at /gluetun).";
      type = types.str;
      example = "/data/apps/gluetun";
    };

    vpnProvider = mkOption {
      description = "VPN service provider (gluetun VPN_SERVICE_PROVIDER).";
      type = types.str;
      default = "mullvad";
    };

    vpnType = mkOption {
      description = "VPN protocol (gluetun VPN_TYPE).";
      type = types.enum ["wireguard" "openvpn"];
      default = "wireguard";
    };

    wireguardAddresses = mkOption {
      description = "Comma-separated wireguard interface addresses.";
      type = types.str;
      example = "10.67.182.174/32";
    };

    ownedOnly = mkOption {
      description = "Restrict to provider-owned servers only (mullvad-specific).";
      type = types.bool;
      default = true;
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

    secretsFile = mkOption {
      description = ''
        Path to environment file containing WIREGUARD_PRIVATE_KEY (and any
        other secret env vars). Must be in KEY=value format. Use
        sops.templates to render from a sops secret.
      '';
      type = types.path;
      example = literalExpression ''config.sops.templates."gluetun-env".path'';
    };
  };

  config = mkIf cfg.enable {
    modules.linux.oci._managedPaths.${cfg.baseDir} = {};

    modules.linux.oci.networks = listToAttrs (
      map (n: nameValuePair n {enable = true;}) cfg.networks
    );

    virtualisation.oci-containers.containers."gluetun" = {
      image = cfg.image;
      environment =
        {
          "VPN_SERVICE_PROVIDER" = cfg.vpnProvider;
          "VPN_TYPE" = cfg.vpnType;
          "WIREGUARD_ADDRESSES" = cfg.wireguardAddresses;
          "OWNED_ONLY" =
            if cfg.ownedOnly
            then "yes"
            else "no";
          "TZ" = cfg.timezone;
        }
        // cfg.extraEnv;
      environmentFiles = [cfg.secretsFile];
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
        ++ (map (n: "--network=${ociLib.networkName n}") cfg.networks);
      log-driver = "journald";
    };

    systemd.services."podman-gluetun" = ociLib.mkServiceConfig {
      networks = cfg.networks;
    };
  };
}
