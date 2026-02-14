{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    inputs.determinate.nixosModules.default
  ];

  # TODO move, make reusable
  nix.settings = {
    substituters = [
      "https://install.determinate.systems"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
    ];
  };

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
      kitty.enable = true;
      ghostty.enable = true;
      _1password = {
        enable = true;
        autostart = true;
      };
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
      age.enable = true;
      sops = {
        enable = true;
        secrets = {
          unsplash = {
            owner = config.user.name;
            group = config.user.group;
            mode = "0440";
          };
          mullvad = {
            owner = config.user.name;
            group = config.user.group;
            mode = "0440";
          };
        };
      };
      claude.enable = true;
    };
    services = {
      zfs.enable = true;
      docker = {
        enable = true;
        enableIPv6 = true;
      };
      sudo-rs.enable = true;
      printing = {
        enable = true;
        client = true;
      };
      gpg.enable = true;
      systemd.modules = {
        sshAgent.enable = true;
        tmp-downloads.enable = true;
      };
      ssh = {
        enable = true;
        client.enable = true;
      };
      yubikey.enable = true;
      syncthing.enable = true;
      mullvad = {
        enable = true;
        enableGUI = true;
      };
      cachix.enable = true;
      netbird = {
        enable = true;
        autoStart = true;
      };
    };
    hardware = {
      zsa.enable = true;
      nvidia = {
        enable = true;
        useOpenDrivers = true;
      };
    };
    desktop = {
      enable = true;
      environment.niri.enable = true;
      random-wallpaper = {
        enable = true;
        token = config.sops.secrets.unsplash.path;
      };
      monitors = ["DP-0"];
      sound.enable = true;
      firefox = {
        enable = true;
        setDefaultPDFViewer = true;
      };
      extraPackages = with pkgs; [
        discord
        signal-desktop-bin
        ungoogled-chromium
        feishin
      ];
      autostart = {
        enable = true;
        entries = [
          "${pkgs.signal-desktop-bin}/share/applications/signal.desktop"
        ];
      };
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages;
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
    useNetworkd = true;

    useDHCP = false;

    interfaces.enp5s0.useDHCP = true;

    networkmanager.enable = true;

    # should only get its ip address from the pihole
    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';
  };

  # TODO implement encrypted home
  # security.pam.zfs = {
  #   enable = true;
  #   homes = "zroot/home";
  # };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
  };
}
