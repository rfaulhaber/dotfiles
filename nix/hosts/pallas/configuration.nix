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
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
  ];

  nixpkgs.overlays = [
    (import ../../overlays/pam_ssh_agent_auth.nix)
  ];

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      docker.enable = true;
      gpg.enable = true;
      sudo-rs.enable = true;
      systemd.modules = {
        dockerCleanup.enable = true;
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
          port = 12981;
        };
      };
      netbird.enable = true;
    };

    themes.active = "moonlight";
  };

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = ["xhci_pci" "usbhid" "usb_storage"];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  # raspberry pi hardware configuration
  hardware = {
    raspberry-pi."4" = {
      fkms-3d.enable = true;
      apply-overlays-dtmerge.enable = true;
    };

    enableRedistributableFirmware = true;
  };

  console.enable = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  networking = {
    hostName = "pallas";
    useDHCP = true;

    defaultGateway6 = {
      address = "2600:1702:6710:117F:56AF:97FF:FE12:496C";
      interface = "end0";
    };

    interfaces.end0 = {
      useDHCP = true;
      ipv6.addresses = let
        addresses = [
          "2600:1702:6710:117F:C40A:AFB1:A677:52E4"
          "2600:1702:6710:117F:487B:3A4C:4399:5458"
          "2600:1702:6710:117F:DA3A:DDFF:FEDA:2B5"
        ];
      in
        builtins.map (address: {
          inherit address;
          prefixLength = 64;
        })
        addresses;
    };

    firewall = {
      enable = true;
      # required for pihole
      allowedTCPPorts = [8085 80 53 67 443];
      allowedUDPPorts = [53 67 68 546 547];
      extraCommands = ''
        iptables -I INPUT 1 -p tcp -m tcp --dport 4711 -i lo -j ACCEPT
        iptables -I INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
      '';
    };
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;
}
