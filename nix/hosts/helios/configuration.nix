# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ../../modules
    ./hardware-configuration.nix
  ];

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      emacs.enable = true;
      neovim.enable = true;
      pcloud.enable = true;
      kitty.enable = true;
      _1password.enable = true;
      git = {
        enable = true;
        useDelta = true;
      };
    };
    services = {
      docker.enable = true;
      samba-mount = {
        enable = true;
        mounts."/home/ryan/calibre" = {
          domain = "192.168.86.10";
          host = "calibre";
        };
      };
      keybase.enable = true;
      gpg.enable = true;
      mail.enable = true;
      redshift.enable = true;
      systemd = {
        enable = true;
        modules = [ "updatedb" ];
      };
      ssh = {
        enable = true;
        enableClient = true;
      };
      passwords.enable = true;
      zerotier = {
        enable = true;
        networks = [ "12ac4a1e719ca283" "b6079f73c6986bc2" ];
      };
    };
    hardware = {
      bluetooth.enable = true;
      zsa.enable = true;
    };
    desktop = {
      bspwm = {
        enable = true;
        extraStartupPrograms = [ "keybase-gui" "pcloud" "discord" ];
        monitors = [ "eDP" ];
      };
      sound.enable = true;
      polybar.enable = true;
      rofi.enable = true;
      random-wallpaper.enable = true;
    };
    langs = {
      js.enable = true;
      rust.enable = true;
      shell.enable = true;
      racket.enable = true;
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
