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
    hostInterfaceName = mkOption {
      description = "Host interface to serve as external interface for NAT.";
      type = types.str;
    };
    mullvadConfig = {
      publicKey = mkOption {
        description = "Mullvad server public key.";
        type = types.str;
      };
      ipv4Address = mkOption {
        description = "IPv4 address for peer node.";
        type = types.str;
      };
      ipv6Address = mkOption {
        description = "IPv6 address for peer node.";
        type = types.str;
        default = null;
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
    networking = {
      nat = {
        enable = true;
        internalInterfaces = ["ve-mullvad"];
        externalInterface = cfg.hostInterfaceName;
      };
    };

    # TODO see https://wiki.nixos.org/wiki/NixOS_Containers#Networking
    # see also https://headless-render-api.com/blog/2024/04/08/mullvad-vpn-containerized-nixos
    # and also https://uint.one/posts/all-internet-over-wireguard-using-systemd-networkd-on-nixos/
    containers.mullvad = {
      privateNetwork = true;
      # arbitrary addresses
      hostAddress = "192.168.100.2";
      localAddress = "192.168.100.11";
      bindMounts.${cfg.privateKeyFile}.isReadOnly = true;

      config = {...}: {
        system.stateVersion = config.system.stateVersion;
        boot.isContainer = true;
        environment.systemPackages = [pkgs.wireguard-tools];
        services.resolved.enable = true;
        networking = {
          hostName = "container-mullvad";
          useHostResolvConf = lib.mkForce false;

          nat = {
            enable = true;
            enableIPv6 = true;
            externalInterface = "eth0";
            internalInterfaces = ["wg0"];
          };

          wg-quick.interfaces.wg0 = let
            hasIPv6Address = cfg.mullvadConfig.ipv6Address != null;
          in {
            address =
              [
                cfg.mullvadConfig.ipv4Address
              ]
              ++ lib.optionals hasIPv6Address [
                cfg.mullvadConfig.ipv6Address
              ];
            privateKeyFile = cfg.privateKeyFile;
            dns = ["10.64.0.1"];

            postUp =
              ''
                ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
                ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s ${cfg.mullvadConfig.ipv4Address} -o eth0 -j MASQUERADE
              ''
              + lib.optionalString hasIPv6Address ''
                ${pkgs.iptables}/bin/ip6tables -A FORWARD -i wg0 -j ACCEPT
                ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s ${cfg.mullvadConfig.ipv6Address} -o eth0 -j MASQUERADE
              '';

            preDown =
              ''
                ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
                ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s ${cfg.mullvadConfig.ipv4Address} -o eth0 -j MASQUERADE
              ''
              + lib.optionalString hasIPv6Address ''
                ${pkgs.iptables}/bin/ip6tables -D FORWARD -i wg0 -j ACCEPT
                ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s ${cfg.mullvadConfig.ipv6Address} -o eth0 -j MASQUERADE
              '';

            peers = [
              {
                publicKey = cfg.mullvadConfig.publicKey;

                allowedIPs = ["0.0.0.0/0"];

                endpoint = "${cfg.mullvadConfig.endpoint}:51820";

                persistentKeepalive = 25;
              }
            ];
          };
        };
      };
    };
  };
}
