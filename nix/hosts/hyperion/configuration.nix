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
    # TODO conditionally import?
    ./hardware-configuration.nix
    inputs.nixos-generators.nixosModules.all-formats
    inputs.lix-module.nixosModules.default
  ];

  modules = {
    programs = {
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
        doomUnstraightened = {
          enable = true;
          setDefault = true;
        };
      };
      neovim.enable = true;
      # how many terminals does a guy need?
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
        plugins = with pkgs.nushellPlugins; [
          polars
        ];
      };
      direnv.enable = true;
      pijul.enable = true;
      age = {
        enable = true;
        secretsDir = ./secrets;
      };
    };
    services = {
      zfs.enable = true;
      docker.enable = true;
      printing = {
        enable = true;
        client = true;
      };
      # samba-mount = {
      #   enable = true;
      #   mounts."${config.user.home}/calibre" = {
      #     domain = "192.168.0.3";
      #     host = "calibre";
      #     # secrets = config.age.secrets.samba.path;
      #   };
      # };
      gpg.enable = true;
      mail.enable = true;
      systemd.modules = {
        sshAgent.enable = true;
        tmp-downloads.enable = true;
      };
      ssh = {
        enable = true;
        client.enable = true;
      };
      # passwords.enable = true;
      yubikey.enable = true;
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
      pueue.enable = true;
    };
    hardware = {
      zsa.enable = true;
      nvidia.enable = true;
    };
    desktop = {
      enable = true;
      environment.niri = lib.mkDefault {enable = true;};
      random-wallpaper = {
        enable = true;
        token = config.age.secrets.unsplash.path;
      };
      monitors = ["DP-0"];
      sound.enable = true;
      firefox = {
        enable = true;
        setDefaultPDFViewer = true;
      };
      extraPackages = with pkgs; [
        discord
        evince
        openvpn
        python3
        signal-desktop-bin
        spotify
        ungoogled-chromium
      ];
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_6_12;
    kernelParams = ["nohibernate"];

    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
    };

    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        windows."windows-11" = {
          title = "Windows 11";
          efiDeviceHandle = "HD0c";
        };
      };

      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
    };

    binfmt.emulatedSystems = ["aarch64-linux"];

    zfs.extraPools = ["zroot"];
  };

  # TODO move, set defaults
  # TODO separate hardware config
  networking = {
    hostName = "hyperion";
    hostId = "836be91c";

    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;

    # should only get its ip address from the pihole
    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';
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
