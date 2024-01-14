# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../../modules
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
      };
      carapace.enable = true;
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      systemd.modules = {
        dockerCleanup.enable = true;
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBvwfQy4U/GVd5S2JhNnaQvuKizbavuUWihmr/89fjZo ryan@hyperion"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDCYJHzbywNiBt3tQGf8Xoi670NU/Gv77Bhzeemr98ro ryan@eos"
          ];
          port = 11689;
        };
      };
    };
  };

  boot = {
    loader = {
      grub.enable = false;
      # Enables the generation of /boot/extlinux/extlinux.conf
      generic-extlinux-compatible.enable = true;
    };

    # necessary for libre computer board
    kernelParams = ["console=tty1" "console=ttyS2,1500000n8"];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking = {
    hostName = "pallas";
    useDHCP = true;
    interfaces.end0.useDHCP = true;

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
