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

  # system = "x86_64-linux";

  modules = {
    programs = {
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
      };
      neovim.enable = true;
      kitty.enable = true;
      wezterm.enable = true;
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
      age = {
        enable = true;
      };
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
      nvidia.enable = true;
    };
    desktop = {
      enable = true;
      environment.bspwm = lib.mkDefault {
        enable = true;
        extraStartupPrograms = ["discord" "1password" "telegram-desktop" "mullvad-gui"];
      };
      random-wallpaper.enable = lib.mkDefault true;
      monitors = ["DP-0"];
      sound.enable = true;
      firefox.enable = true;
      extraPackages = with pkgs; [
        chromium
        discord
        evince
        gnome-screenshot
        openvpn
        python3
        signal-desktop
        spotify
        tdesktop
      ];
    };
    themes.active = "tokyo-night";
  };

  specialisation = {
    hyprland.configuration = {
      modules.desktop = {
        enable = true;
        environment.bspwm.enable = false;
        environment.hyprland.enable = true;
        random-wallpaper.enable = false;
      };
    };
  };

  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelParams = [ "nohibernate" ];

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

    zfs = {
      extraPools = [ "zroot" ];
    };
  };

  # TODO move, set defaults
  networking = {
    hostName = "hyperion";
    hostId = "836be91c";

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };

  # for use when making a vm using nixos-rebuild build-vm
  # note that these options aren't respected when using nixos-generate
  # virtualisation.vmVariant = {
  #   user.initialHashedPassword = "$6$GNqgrpQokCNs9sfr$vjVC5sv1rfLElOCY/czFKLR7gcoQQgoLR/l0X7I7KhgCKqaoYuUWlgyfCdFeRdFJtkckDFoiEkDoBIflMIEQR1"; # "test" lol

  #   # the vm will try using DHCP for an interface that doesn't exist
  #   networking.interfaces.enp5s0.useDHCP = lib.mkForce false;
  #   networking.interfaces.eth0.useDHCP = true;
  #   networking.interfaces.br0.useDHCP = true;
  #   networking.bridges = {
  #     "br0" = {
  #       interfaces = ["eth0"];
  #     };
  #   };

  #   networking.networkmanager.enable = lib.mkForce false;
  # };
}
