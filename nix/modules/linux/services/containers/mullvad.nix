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
    networking = let
      interfaceName = "br0";
    in {
      bridges.${interfaceName}.interfaces = [
        "enp5s0"
      ];
      interfaces.${interfaceName} = {
        useDHCP = true;
      };
    };

    # TODO see https://wiki.nixos.org/wiki/NixOS_Containers#Networking
    containers.mullvad = {
      # We need a private network to isolate it from the host
      privateNetwork = true;
      # Define the IPs for the virtual ethernet (veth) pair
      # Host side: 10.23.0.1
      # Container side (host0 interface): 10.23.0.2
      # hostAddress = "10.23.0.1";
      localAddress = "192.168.0.225";
      hostBridge = "br0";
      bindMounts.${cfg.privateKeyFile}.isReadOnly = true;

      # This is the configuration for the OS *inside* the container
      config = {...}: {
        system.stateVersion = "24.05"; # Or your host's version
        boot.isContainer = true;
        networking = {
          hostName = "container-mullvad";
          # Use systemd-resolved inside the container
          # Workaround for bug https://github.com/NixOS/nixpkgs/issues/162686
          useHostResolvConf = lib.mkForce false;
          useDHCP = false;
          interfaces.eth0.useDHCP = true;
        };

        services.resolved.enable = true;
        environment.systemPackages = [pkgs.wireguard-tools];
      };
    };
  };
}
