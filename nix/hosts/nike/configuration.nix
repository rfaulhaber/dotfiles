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
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
  ];

  modules = {
    desktop = {
      enable = true;
      environment.retroarch.enable = true;
    };
    programs = {
      age = {
        enable = true;
        secrets = {
          userPasswd.file = ./secrets/passwd.age;
          samba.file = ./secrets/samba.age;
        };
      };
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      gpg.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ2W7QJTJ5LtlPoEVK3o6S45+7wfn4ECnt7UNRUACRYU ryan@hyperion"
          ];
          port = 14625;
        };
      };
      samba-mount = {
        enable = true;
        mounts."${config.user.home}/games" = {
          domain = "192.168.0.3";
          host = "games";
          secrets = config.age.secrets.samba.path;
        };
      };
    };

    themes.active = "moonlight";
  };

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi3;
    initrd.availableKernelModules = ["xhci_pci" "usbhid" "usb_storage"];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  console.enable = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  networking = {
    hostName = "nike";
    useDHCP = true;
    interfaces.end0.useDHCP = true;
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

  user.hashedPasswordFile = config.age.secrets.userPasswd.path;
}
