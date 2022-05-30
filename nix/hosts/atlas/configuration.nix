# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let homeDomain = "homelab.lan";
in {
  imports = [
    ../../modules
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      neovim.enable = true;
      git.enable = true;
      exa.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      virt.enable = true;
      # TODO this is required but it shouldn't be
      # TODO make these use constants of some kind, e.g. with systemdModules; [ docker-cleanup ]
      systemd = {
        enable = true;
        modules = [ "updatedb" "docker-cleanup" ];
      };
      ssh = {
        enable = true;
        enableServer = true;
        keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJHGYKXsTAHFyfjUzynjFd1XcfnL3tO90v/2BzSBq3yK ryf@sent.as"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJV6DYWf3PwOlEvVPoW2FEeVw6tHdeWIW9o2/qqWsNJE ryf@sent.as"
        ];
      };
      snapraid = {
        enable = true;
        parityFiles = [ "/diskp/snapraid.parity" ];
        dataDisks = {
          disk1 = "/disk1/";
          disk2 = "/disk2/";
          disk3 = "/disk3/";
        };
        contentFiles =
          [ "/var/snapraid/snapraid.content" "/diskp/snapraid.content" ];
      };
      mergerfs = {
        enable = true;
        branches = [ "/disk1" "/disk2" "/disk3" ];
        target = "/data";
      };
      samba-serve = {
        enable = true;
        shares.calibre = {
          path = /data/calibre;
          comment = "Calibre share.";
        };
      };
      zerotier = {
        enable = true;
        networks = [ "12ac4a1e719ca283" "b6079f73c6986bc2" ];
        dockerWhitelist = [ 8089 ];
        sharedNetworkInterface = "ztbto5nphs";
      };
    };
    themes.active = "city-lights";
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    tmpOnTmpfs = true;
    cleanTmpDir = true;

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
      allowedTCPPorts = [ 8085 80 53 67 ];
      allowedUDPPorts = [ 53 67 68 546 547 ];
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
