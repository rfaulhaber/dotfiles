{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.zerotier;
in {
  options.modules.services.zerotier = {
    enable = mkEnableOption false;
    networks = mkOption {
      description = "List of networks to join.";
      type = types.listOf types.str;
      default = [ ];
    };
    port = mkOption {
      description = "Port to be used by ZeroTier";
      type = types.int;
      default = 9993;
    };
    # these are special as they require special iptables rules.
    # if we were whitelisting local ports, we wouldn't require such complicated rules!
    dockerWhitelist = mkOption {
      description =
        "Docker ports to allow access to on the ZeroTier network. All others are blocked.";
      type = types.listOf types.int;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
      port = cfg.port;
    };

    networking.firewall.extraCommands = let
      # a thousand thanks to this ServerFault answer:
      # https://serverfault.com/questions/704643/steps-for-limiting-outside-connections-to-docker-container-with-iptables/933803
      mkRule = port:
        "iptables -I DOCKER-USER -i zt+ -p tcp -m conntrack --ctorigdstport ${
          toString (port)
        } --ctdir ORIGINAL -j ACCEPT";
      whitelist = map mkRule cfg.dockerWhitelist;
      blockAll = [
        # "iptables -A INPUT -i zt+ -j DROP"
        "iptables -I DOCKER-USER -i zt+ -j DROP"
      ];
    in concatStringsSep "\n" (blockAll ++ whitelist);
  };
}
