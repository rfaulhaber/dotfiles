{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules
    ./hardware-configuration.nix
    inputs.determinate.nixosModules.default
  ];

  nix.settings = {
    substituters = ["https://install.determinate.systems"];
    trusted-public-keys = ["cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="];
  };

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
      sudo-rs.enable = true;
      docker = {
        enable = true;
        enableNvidiaTools = true;
      };
      gpg.enable = true;
      systemd.modules = {
        updatedb.enable = true;
        dockerCleanup.enable = true;
        # backupDockerConfig.enable = true;
      };
      printing = {
        enable = true;
        server = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          extraConfig = ''
            MaxStartups 30:30:60
          '';
        };
      };
      netbird.enable = true;
    };

    # NOTE: the open drivers do not work on atlas
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
    kernelPackages = pkgs.linuxPackages;
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
