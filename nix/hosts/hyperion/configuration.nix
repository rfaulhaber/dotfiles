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
      "http://vulcan.lan:4965"
      "https://nixos-raspberrypi.cachix.org"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

  nix.extraOptions = ''
    !include ${config.sops.templates."nix-access-tokens.conf".path}
  '';

  sops.templates."nix-access-tokens.conf" = {
    content = ''
      access-tokens = github.com=${config.sops.placeholder.github}
    '';
    owner = config.user.name;
    group = config.user.group;
    mode = "0440";
  };

  modules = {
    programs = {
      btop.enable = true;
      emacs = {
        enable = true;
        package = pkgs.emacs-git;
        doomUnstraightened = {
          enable = true;
          setDefault = true;
        };
      };
      kitty.enable = true;
      ghostty = {
        enable = true;
        fontSize = 20;
      };
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
          # used by nix.extraOptions to set access-tokens for github.com
          # and avoid rate limits when fetching flake inputs
          github = {};
          "netbird/setup-key" = {};
        };
      };
      claude.enable = true;
    };
    services = {
      zfs.enable = true;
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
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
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
      environment.niri = {
        enable = true;
        outputs = {
          "DP-3" = {
            mode = {
              width = 2560;
              height = 1440;
              refresh = 180.063;
            };
            scale = 1;
            transform.rotation = 0;
            position = {
              x = 0;
              y = 0;
            };
          };
          "DP-1" = {
            mode = {
              width = 3840;
              height = 2160;
              refresh = 59.997;
            };
            scale = 1;
            transform.rotation = 0;
            position = {
              x = 2560;
              y = 0;
            };
          };
        };
      };
      random-wallpaper = {
        enable = true;
        perDisplay = true;
        token = config.sops.secrets.unsplash.path;
      };
      monitors = ["DP-3" "DP-1"];
      sound.enable = true;
      firefox = {
        enable = true;
        setDefaultPDFViewer = true;
      };
      extraPackages = with pkgs; [
        signal-desktop
        ungoogled-chromium
        feishin
        inputs.rz.packages.${pkgs.stdenv.hostPlatform.system}.with-xz2-bzip2
      ];
      autostart = {
        enable = true;
        entries = [
          "${pkgs.signal-desktop}/share/applications/signal.desktop"
        ];
      };
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelParams = ["nohibernate"];

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

    zfs = {
      extraPools = ["zroot"];
      forceImportRoot = false;
    };
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
}
