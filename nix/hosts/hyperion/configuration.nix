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
      systemd.modules = with lib.my.systemdModules; [sshAgent];
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
      awesome.enable = true;
      # bspwm = {
      #   enable = true;
      #   extraStartupPrograms = ["keybase-gui" "discord" "1password" "telegram-desktop"];
      #   monitors = ["DP-0"];
      # };
      sound.enable = true;
      # polybar.enable = true;
      # rofi.enable = true;
      random-wallpaper.enable = true;
      firefox.enable = true;
    };
    themes.active = "moonlight";
  };

  boot = {
    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
    };

    loader = {
      grub = {
        enable = true;
        useOSProber = true;
        efiSupport = true;
        device = "nodev";
        gfxmodeEfi = "1024x768";
      };

      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
    };
  };

  # TODO move, set defaults
  networking = {
    hostName = "hyperion";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;
  };

  hardware = {
    opengl.enable = true;
    nvidia.modesetting.enable = true;
  };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };
}
