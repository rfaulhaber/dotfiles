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
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAZQ6dhGnjyJ+SBMeN5IRHcpV6ERR+a/WPmvD7o2TM90 ryan@hyperion"
          ];
          extraConfig = ''
            MaxStartups 30:30:60
          '';
        };
      };
      snapraid = {
        enable = true;
        parityFiles = ["/diskp/snapraid.parity"];
        dataDisks = {
          disk1 = "/disk1/";
          disk2 = "/disk2/";
          disk3 = "/disk3/";
          disk4 = "/disk4/";
          disk5 = "/disk5/";
          disk6 = "/disk6/";
          disk7 = "/disk7/";
        };
        contentFiles = ["/var/snapraid/snapraid.content" "/diskp/snapraid.content"];
      };
      mergerfs = {
        enable = true;
        branches = ["/disk1" "/disk2" "/disk3" "/disk4" "/disk5" "/disk6" "/disk7"];
        target = "/data";
      };
      samba-serve = {
        enable = true;
        shares = {
          calibre = {
            path = "/data/calibre";
            comment = "Calibre share.";
          };
          games = {
            path = "/data/games";
            comment = "Retroarch roms";
          };
        };
      };
      zerotier = {
        enable = true;
        networks = ["12ac4a1e719ca283" "b6079f73c6986bc2"];
        sharedNetworkConfig = {
          "ztbto5nphs" = {
            dockerWhitelist = [8089];
          };
        };
      };
    };
    themes.active = "city-lights";
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
  };

  networking = {
    hostName = "atlas";

    useDHCP = false;

    interfaces = {
      eno1.useDHCP = true;
      eno2.useDHCP = true;
    };

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
