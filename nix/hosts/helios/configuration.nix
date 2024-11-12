# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ../../modules
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t495
  ];

  # system = "x86_64-linux";

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      emacs.enable = true;
      neovim.enable = true;
      kitty = {
        enable = true;
        fontSize = 16;
      };
      _1password.enable = true;
      git = {
        enable = true;
        useDelta = true;
      };
      eza.enable = true;
    };
    services = {
      docker.enable = true;
      samba-mount = {
        enable = true;
        mounts."${config.user.home}/calibre" = {
          domain = "192.168.86.10";
          host = "calibre";
        };
      };
      keybase.enable = true;
      gpg.enable = true;
      mail.enable = true;
      redshift.enable = true;
      systemd.modules = {
        sshAgent.enable = true;
      };
      ssh = {
        enable = true;
        client.enable = true;
      };
      passwords.enable = true;
      zerotier = {
        enable = true;
        networks = ["b6079f73c6986bc2"];
      };
    };
    hardware = {
      bluetooth.enable = true;
      zsa.enable = true;
    };
    desktop = {
      enable = true;
      environment.bspwm = {
        enable = true;
        extraStartupPrograms = ["keybase-gui" "discord"];
      };
      monitors = ["eDP"];
      sound.enable = true;
      polybar = {
        enable = true;
        fontSize = 12;
      };
      rofi.enable = true;
      random-wallpaper.enable = true;
      laptop.enable = true;
      firefox = {
        enable = true;
        setDefaultBrowser = true;
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
    hostName = "helios";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces.wlp1s0.useDHCP = true;

    networkmanager.enable = true;
  };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };
}
