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
    ./oci.nix
    inputs.determinate.nixosModules.default
  ];

  nix.settings = {
    substituters = [
      "https://install.determinate.systems"
      "http://vulcan.lan:4965"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
    ];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
      };
    };
    services = {
      zfs.enable = true;
      sudo-rs.enable = true;
      systemd.modules = {
        updatedb.enable = true;
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
      samba.serve = {
        enable = true;
        subnet = "192.168.0.";
        interface = "eno2";
        shares.roms = {
          path = "/data/games/roms/roms";
          comment = "ROM library";
          readOnly = true;
          guestOk = true;
        };
      };
      nfs.serve = {
        enable = true;
        interface = "eno2";
        exports = {
          movies = {
            path = "/data/movies";
            clients = "192.168.0.105(ro,sync,no_subtree_check,no_root_squash)";
          };
          tv = {
            path = "/data/tv";
            clients = "192.168.0.105(ro,sync,no_subtree_check,no_root_squash)";
          };
          llm-models = {
            path = "/data/llm/models";
            clients = "192.168.0.105(rw,sync,no_subtree_check,no_root_squash)";
          };
        };
      };
    };

    # NOTE: the open drivers do not work on atlas (GTX 1050 Ti, Pascal)
    hardware.nvidia = {
      enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };

    themes.active = "tokyo-night-dark";
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.device = "nodev";
    };

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

    interfaces = {
      eno1.useDHCP = true;
      eno2.useDHCP = true;
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
}
