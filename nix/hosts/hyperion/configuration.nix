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
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
      };
      neovim.enable = true;
      kitty.enable = true;
      _1password.enable = true;
      git = {
        enable = true;
        useDelta = true;
      };
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      direnv.enable = true;
      pijul.enable = true;
      age.enable = true;
    };
    services = {
      doas.enable = true;
      docker.enable = true;
      samba-mount = {
        enable = true;
        mounts."${config.user.home}/calibre" = {
          domain = "192.168.0.3";
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
      syncthing.enable = true;
      mullvad = {
        enable = true;
        enableGUI = true;
      };
      cachix.enable = true;
    };
    hardware = {
      zsa.enable = true;
    };
    desktop = {
      enable = true;
      environment.bspwm = {
        enable = true;
        extraStartupPrograms = ["discord" "1password" "telegram-desktop" "mullvad-gui"];
      };
      # environment.sway.enable = true;
      videoDrivers = ["nvidia"];
      monitors = ["DP-0"];
      sound.enable = true;
      random-wallpaper.enable = true;
      firefox.enable = true;
    };
    themes.active = "moonlight";
  };

  boot = {
    # use the latest kernel
    kernelPackages = pkgs.linuxPackages_latest;

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

    binfmt.emulatedSystems = ["aarch64-linux"];
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
    graphics.enable = true;
    nvidia = {
      modesetting.enable = true;
      open = true;
    };
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };

  # TODO messy, clean up
  # nix.gc.automatic = lib.mkDefault false;

  # for use when making a vm
  # TODO make able to detect when building for a vm
  # TODO use agenix
  # user.initialHashedPassword = "$6$ucPFz/HD9WLviVXr$t8tpRtNT649hIJV7W5gaR.HeTidipk6wGAiT0rLOQYeX6YDurg7GZ3gR8EX/tByq2mmy1U5L/5RJIjr1oglpE."; # "test"
}
