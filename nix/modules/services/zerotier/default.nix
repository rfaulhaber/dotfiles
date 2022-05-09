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

    sharedNetworkInterface = mkOption {
      description =
        "Name of the ZeroTier interface that requires firewall rules.";
      type = types.str;
      # TODO for some reason this won't build without a default value
      default = "zt+";
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = if ((length cfg.dockerWhitelist) > 0) then
        cfg.sharedNetworkInterface != null
      else
        true;
      message = "sharedNetworkInterface is required.";
    }];

    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
      port = cfg.port;
    };

    networking.firewall = mkIf (cfg.sharedNetworkInterface != "") {
      extraCommands = let
        # a thousand thanks to this ServerFault answer:
        # https://serverfault.com/questions/704643/steps-for-limiting-outside-connections-to-docker-container-with-iptables/933803
        mkRule = port:
          "iptables -I DOCKER-USER -i ${cfg.sharedNetworkInterface} -p tcp -m conntrack --ctorigdstport ${
            toString (port)
          } --ctdir ORIGINAL -j ACCEPT";
        whitelist = map mkRule cfg.dockerWhitelist;
        blockAll = [
          "iptables -I DOCKER-USER -i ${cfg.sharedNetworkInterface} -j DROP"
        ];
      in concatStringsSep "\n" (blockAll ++ whitelist);
    };
  };
}
