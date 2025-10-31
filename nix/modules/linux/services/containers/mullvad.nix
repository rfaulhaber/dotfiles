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
        description = "IP address for peer node.";
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
    # Configure the container

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
        address = ["10.23.0.1/24"];
        networkConfig = {
          IPv4Forwarding = true;
          IPMasquerade = "ipv4";
          # Don't wait for this interface to be "online"
          ConfigureWithoutCarrier = true;
        };
        # Ignore this interface for wait-online
        linkConfig.RequiredForOnline = "no";
      };
    };

    # TODO see https://wiki.nixos.org/wiki/NixOS_Containers#Networking
    containers.mullvad-vpn = {
      # We need a private network to isolate it from the host
      privateNetwork = true;
      # Define the IPs for the virtual ethernet (veth) pair
      # Host side: 10.23.0.1
      # Container side (host0 interface): 10.23.0.2
      hostAddress = "10.23.0.1";
      localAddress = "10.23.0.2";
      bindMounts.${cfg.privateKeyFile}.isReadOnly = true;

      # This is the configuration for the OS *inside* the container
      config = {...}: {
        system.stateVersion = "24.05"; # Or your host's version
        boot.isContainer = true;

        networking = {
          useHostResolvConf = lib.mkForce false;
          firewall.enable = false;
          nameservers = ["10.64.0.1"]; # Mullvad DNS

          wg-quick.interfaces = {
            wg0 = {
              address = cfg.mullvadConfig.addresses;
              # use dnscrypt, or proxy dns as described above
              dns = ["10.64.0.1"];
              privateKeyFile = cfg.privateKeyFile;
              peers = [
                {
                  # bt wg conf
                  publicKey = cfg.mullvadConfig.publicKey;
                  allowedIPs = [
                    "0.0.0.0/0"
                  ];
                  endpoint = "${cfg.mullvadConfig.endpoint}:51820";
                  persistentKeepalive = 25;
                }
              ];
            };
          };
        };

        systemd.services."wg-quick-wg-mullvad" = {
          wantedBy = ["multi-user.target"];
          after = ["network.target"];
        };

        environment.systemPackages = [pkgs.wireguard-tools];
      };
    };

    systemd.services."container@mullvad-vpn" = {
      after = ["systemd-networkd.service"];
      wants = ["systemd-networkd.service"];
    };

    # Tell systemd-networkd-wait-online to ignore the bridge
    systemd.services.systemd-networkd-wait-online = {
      serviceConfig = {
        ExecStart = [
          "" # Clear the default
          "${config.systemd.package}/lib/systemd/systemd-networkd-wait-online --ignore=br-mullvad"
        ];
      };
    };
  };
}
