{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules
    ./hardware.nix
    inputs.disko.nixosModules.disko
    ./disko.nix
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      systemd.modules = {
        updatedb.enable = true;
        dockerCleanup.enable = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 6674;
        };
      };
      netbird.enable = true;
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    loader.grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };

  networking = {
    hostName = "janus";
    hostId = "66a2b43a";

    interfaces.ens3 = {
      useDHCP = true;
      ipv4.addresses = [
        {
          address = "66.63.168.153";
          prefixLength = 24;
        }
        {
          address = "66.63.168.244";
          prefixLength = 24;
        }
      ];
    };

    firewall = {
      enable = true;
      # SSH
      allowedTCPPorts = [6674];
      # Coturn (TURN/STUN) uses network_mode: host, so it needs INPUT chain rules.
      # Other Docker containers use port forwarding (FORWARD chain), which NixOS
      # firewall does not touch — they manage their own iptables rules.
      allowedUDPPorts = [3478 5349];
      allowedUDPPortRanges = [
        {
          from = 49152;
          to = 65535;
        }
      ];
    };
  };
}
