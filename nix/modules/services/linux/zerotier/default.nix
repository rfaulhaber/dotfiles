{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.zerotier;
in {
  options.modules.services.zerotier = {
    enable = mkEnableOption false;
    networks = mkOption {
      description = "List of networks to join.";
      type = types.listOf types.str;
      default = [];
    };
    port = mkOption {
      description = "Port to be used by ZeroTier";
      type = types.int;
      default = 9993;
    };
    # these are special as they require special iptables rules.
    # if we were whitelisting local ports, we wouldn't require such complicated rules!
    dockerWhitelist = mkOption {
      description = "Docker ports to allow access to on the ZeroTier network. All others are blocked.";
      type = types.listOf types.int;
      default = [];
    };

    sharedNetworkInterface = mkOption {
      description = "Name of the ZeroTier interface that requires firewall rules.";
      type = types.str;
      # TODO for some reason this won't build without a default value
      default = "zt+";
    };

    sharedNetworkConfig = mkOption {
      description = "Shared networks and their per-network config.";
      type = types.attrsOf (types.submodule {
        options = {
          dockerWhitelist = mkOption {
            description = "Docker ports to allow access to on the ZeroTier network. All others are blocked.";
            type = types.listOf types.int;
            default = [];
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
      port = cfg.port;
    };

    # Linux-specific firewall configuration
    networking.firewall = mkIf (pkgs.stdenv.isLinux && (length (attrNames cfg.sharedNetworkConfig)) > 0) {
      extraCommands = let
        # a thousand thanks to this ServerFault answer:
        # https://serverfault.com/questions/704643/steps-for-limiting-outside-connections-to-docker-container-with-iptables/933803
        mkRulesForInterface = interface: interfaceAttrs: map (port: mkRule interface port) interfaceAttrs.dockerWhitelist;
        mkRule = interface: port: "iptables -I DOCKER-USER -i ${interface} -p tcp -m conntrack --ctorigdstport ${toString port} --ctdir ORIGINAL -j ACCEPT";
        whitelist =
          if (cfg.sharedNetworkConfig != null)
          then flatten (attrValues (mapAttrs mkRulesForInterface cfg.sharedNetworkConfig))
          else [];
        blockAll =
          if (cfg.sharedNetworkConfig != null)
          then map (interface: "iptables -I DOCKER-USER -i ${interface} -j DROP") (attrNames cfg.sharedNetworkConfig)
          else [];
      in ''
        # DEBUG: custom zerotier rules:
        ${concatStringsSep "\n" (blockAll ++ whitelist)}
      '';
    };
  };
}
