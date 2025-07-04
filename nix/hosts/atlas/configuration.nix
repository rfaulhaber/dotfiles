# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [../../modules ./hardware-configuration.nix];

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
      zfs.enable = true;
      docker = {
        enable = true;
        enableNvidiaTools = true;
      };
      gpg.enable = true;
      systemd.modules = {
        updatedb.enable = true;
        dockerCleanup.enable = true;
      };
      printing = {
        enable = true;
        server = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhN+t0aI3pQhZsFPoRn8dWe7YvDn3ehWOUmwvqbQyZP ryan@1p"
          ];
          extraConfig = ''
            MaxStartups 30:30:60
          '';
        };
      };
      netbird.enable = true;
    };

    hardware.nvidia.enable = true;

    themes.active = "tokyo-night-dark";
  };

  boot = {
    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
    };

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.device = "nodev";
    };

    # kernel settings for zfs
    kernelPackages = pkgs.linuxPackages_6_12;
    kernelParams = ["nohibernate"];

    zfs = {
      extraPools = ["system" "data"];
    };
  };

  networking = {
    hostName = "atlas";
    hostId = "d6acc614";

    useDHCP = false;

    # vlans = {
    #   "docker_bridge" = {
    #     id = 100;
    #     interface = "eno2";
    #   };
    # };

    # macvlans.docker_bridge = {
    #   interface = "eno2";
    #   mode = "bridge";
    # };

    interfaces = {
      eno1.useDHCP = true;
      eno2.useDHCP = true;

      # docker_bridge = {
      #   useDHCP = false;
      #   ipv4.addresses = [
      #     {
      #       address = "192.168.0.220";
      #       prefixLength = 24;
      #     }
      #   ];
      # };
    };

    # should only get its static ip address from the pihole
    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';

    firewall = {
      enable = true;
      # required for pihole
      allowedTCPPorts = [8085 80 53 67];
      allowedUDPPorts = [53 67 68 546 547];
      extraCommands = ''
        iptables -I INPUT 1 -p tcp -m tcp --dport 4711 -i lo -j ACCEPT
        iptables -I INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
      '';
    };
  };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };
}
