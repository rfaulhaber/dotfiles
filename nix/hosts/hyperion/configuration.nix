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
  imports = [../../modules ./hardware-configuration.nix];

  modules = {
    programs = {
      zsh = {
        enable = true;
        # setDefault = true;
        ohMyZsh = {
          enable = true;
          theme = "agnoster";
        };
      };
      # starship.enable = true;
      emacs.enable = true;
      neovim.enable = true;
      kitty.enable = true;
      _1password.enable = true;
      git = {
        enable = true;
        useDelta = true;
      };
      exa.enable = true;
      nushell = {
        enable = true;
        setDefault = true;
      };
      direnv.enable = true;
    };
    services = {
      docker.enable = true;
      samba-mount = {
        enable = true;
        mounts."${config.user.home}/calibre" = {
          domain = "192.168.0.2";
          host = "calibre";
        };
      };
      keybase.enable = true;
      gpg.enable = true;
      mail.enable = true;
      redshift.enable = true;
      systemd.modules = with lib.my.systemdModules; [updatedb sshAgent];
      ssh = {
        enable = true;
        enableClient = true;
      };
      passwords.enable = true;
      wireguard.enable = true;
      zerotier = {
        enable = true;
        networks = ["b6079f73c6986bc2"];
      };
      syncthing.enable = true;
    };
    hardware = {
      zsa.enable = true;
    };
    desktop = {
      enable = true;
      videoDrivers = ["nvidia"];
      # bspwm = {
      #   enable = true;
      #   extraStartupPrograms = ["keybase-gui" "discord" "1password" "telegram-desktop"];
      #   monitors = ["DP-0"];
      # };
      sound.enable = true;
      # polybar.enable = true;
      # rofi.enable = true;
      hyprland.enable = true;
      random-wallpaper.enable = true;
      firefox.enable = true;
    };
    themes.active = "moonlight";
  };

  boot.tmp = {
    useTmpfs = true;
    cleanOnBoot = true;
  };

  boot.loader = {
    grub = {
      enable = true;
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
    hostName = "hyperion";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;

  # List services that you want to enable:

  # services = { printing.enable = true; };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };
}
