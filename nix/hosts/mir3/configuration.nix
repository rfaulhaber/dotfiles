# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ../../modules
    ./hardware-configuration.nix
    # <home-manager/nixos>
  ];

  modules = {
    programs = {
      zsh = {
        enable = true;
        setDefault = true;
      };
      emacs.enable = true;
      pcloud.enable = true;
    };
    services = {
      docker.enable = true;
      calibre-mount = {
        enable = true;
        mountPoint = "/home/ryan/calibre";
      };
      keybase.enable = true;
    };
    hardware = { bluetooth.enable = true; };
    desktop = {
      bspwm = {
        enable = true;
        extraStartupPrograms =
          [ "keybase-gui" "pcloud" "discord" "telegram-desktop" ];
      };
      polybar.enable = true;
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

  location.provider = "geoclue2";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # TODO clean up!
  environment.systemPackages = with pkgs; [

    #desktop
    betterlockscreen
    chromium
    discord
    evince
    feh
    firefox-devedition-bin
    gnome3.gnome-screenshot
    kitty
    keychain
    openvpn
    pass
    redshift
    rofi
    spotify
    tdesktop
    xscreensaver
    xtitle
    wally-cli

    #dev

    #dev.util
    git
    shellcheck
    shfmt
    gnumake
    just

    #dev.rust
    rustup

    #dev.racket
    racket

    #dev.editors
    neovim

    #util
    cifs-utils
    texlive.combined.scheme-medium
    kvm
    qemu
    qemu-utils

    #system deps
    python3
    libsecret
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.evince.enable = true;

  programs.seahorse.enable = true;

  # List services that you want to enable:

  services = {
    printing.enable = true;

    # it is unclear to me how to automatically unlock gnome keyring upon login, so
    # I'm taking the shotgun approach
    gnome = { gnome-keyring.enable = true; };
    dbus.packages = with pkgs; [ gnome.gnome-keyring gcr gnome3.dconf ];

    redshift = {
      enable = true;
      brightness = {
        day = "1";
        night = "1";
      };
      temperature = {
        day = 5700;
        night = 2600;
      };
    };

    udev = {
      extraRules = ''
        SUBSYSTEM=="usb", ATTR{idVendor}=="3297", ATTR{idProduct}=="1969", GROUP="plugdev"
      '';
    };
  };

  security.pam.services.lightdm.enableGnomeKeyring = true;

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
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

