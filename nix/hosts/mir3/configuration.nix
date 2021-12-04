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
      calibre-mount = {
        enable = true;
        mountPoint = "/home/ryan/calibre";
      };
      keybase.enable = true;
      gpg.enable = true;
      mail.enable = true;
      redshift.enable = true;
      systemd.modules = [ "updatedb" ];
      virt.enable = true;
    };
    hardware = {
      bluetooth.enable = true;
      zsa.enable = true;
    };
    desktop = {
      bspwm = {
        enable = true;
        extraStartupPrograms =
          [ "keybase-gui" "pcloud" "discord" "telegram-desktop" ];
      };
      polybar.enable = true;
      rofi.enable = true;
      random-wallpaper.enable = true;
    };
    # TODO change to list?
    langs = {
      js.enable = true;
      rust.enable = true;
      shell.enable = true;
      racket.enable = true;
    };
    themes.active = "city-lights";
  };

  nixpkgs.config.allowUnfree = true;

  boot.tmpOnTmpfs = true;
  boot.cleanTmpDir = true;
  boot.loader = {
    systemd-boot.enable = true;
    grub = {
      enable = true;
      version = 2;
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };

    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
  };

  networking = {
    hostName = "mir3";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces = { enp5s0 = { useDHCP = true; }; };

    networkmanager.enable = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # TODO clean up!
  environment.systemPackages = with pkgs; [
    #dev.util
    gnumake
    just

    #util
    cifs-utils
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;

  # List services that you want to enable:

  # services = { printing.enable = true; };

  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  system = {
    autoUpgrade = {
      enable = true;
      allowReboot = true;
    };
    stateVersion = "21.11";
  };
}
