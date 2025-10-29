{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.containers.mullvad;
in {
  options.modules.services.containers.mullvad = {
    enable = mkEnableOption false;
    privateKeyFile = mkOption {
      description = "Path to Wireguard private key file.";
      type = types.oneOf [types.path types.str];
    };
    mullvadConfig = {
      publicKey = mkOption {
        description = "Mullvad server public key.";
        type = types.str;
      };
      addresses = mkOption {
        description = "IP addresses for peer node.";
        type = types.listOf types.str;
        default = [];
      };
      endpoint = mkOption {
        description = "Mullvad endpoint, without port.";
        type = types.str;
      };
    };
  };

  config = mkIf cfg.enable {
    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };

    systemd.network = {
      enable = true;

      netdevs."10-br-mullvad" = {
        netdevConfig = {
          Kind = "bridge";
          Name = "br-mullvad";
        };
      };

      networks."10-br-mullvad" = {
        matchConfig.Name = "br-mullvad";
        address = ["10.233.1.1/24"];
        networkConfig = {
          IPForward = true;
          IPMasquerade = "ipv4";
        };
      };
    };

    # Configure the container
    containers.mullvad-vpn = {
      autoStart = true;
      privateNetwork = true;
      hostBridge = "br-mullvad";
      localAddress = "10.233.1.2/24";

      config = {
        config,
        pkgs,
        ...
      }: {
        system.stateVersion = "24.05";

        networking = {
          firewall.enable = false;
          useHostResolvConf = false;
          nameservers = ["10.64.0.1"];
        };

        environment.systemPackages = with pkgs; [
          wireguard-tools
          iptables
          iproute2
        ];

        networking.wg-quick.interfaces = {
          wg-mullvad = {
            address = cfg.mullvadConfig.addresses;
            dns = ["10.64.0.1"];

            privateKeyFile = cfg.privateKeyFile;

            peers = [
              {
                publicKey = cfg.mullvadConfig.publicKey;
                allowedIPs = ["0.0.0.0/0" "::/0"];
                endpoint = "${cfg.mullvadConfig.endpoint}:51820";
                persistentKeepalive = 25;
              }
            ];

            postUp = ''
              ${pkgs.iptables}/bin/iptables -I OUTPUT ! -o ${pkgs.wireguard-tools}/bin/wg-mullvad -m mark ! --mark $(${pkgs.wireguard-tools}/bin/wg show ${pkgs.wireguard-tools}/bin/wg-mullvad fwmark) -m addrtype ! --dst-type LOCAL ! -d 192.168.0.0/16 -j REJECT && ip6tables -I OUTPUT ! -o ${pkgs.wireguard-tools}/bin/wg-mullvad -m mark ! --mark $(${pkgs.wireguard-tools}/bin/wg show ${pkgs.wireguard-tools}/bin/wg-mullvad fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
            '';

            preDown = ''
              ${pkgs.iptables}/bin/iptables -D OUTPUT ! -o ${pkgs.wireguard-tools}/bin/wg-mullvad -m mark ! --mark $(${pkgs.wireguard-tools}/bin/wg show ${pkgs.wireguard-tools}/bin/wg-mullvad fwmark) -m addrtype ! --dst-type LOCAL ! -d 192.168.0.0/16 -j REJECT && ip6tables -D OUTPUT ! -o ${pkgs.wireguard-tools}/bin/wg-mullvad -m mark ! --mark $(${pkgs.wireguard-tools}/bin/wg show ${pkgs.wireguard-tools}/bin/wg-mullvad fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
            '';
          };
        };

        systemd.services."wg-quick-wg-mullvad" = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
        };
      };
    };

    systemd.services."container@mullvad-vpn" = {
      after = ["systemd-networkd.service"];
      wants = ["systemd-networkd.service"];
    };
  };
}
