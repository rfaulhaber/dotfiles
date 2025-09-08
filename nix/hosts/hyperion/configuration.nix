# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  inputs,
  system,
  ...
}: {
  imports = [
    # TODO conditionally import?
    ./hardware-configuration.nix
    inputs.nixos-generators.nixosModules.all-formats
    inputs.determinate.nixosModules.default
  ];

  # TODO move
  nix.settings = {
    substituters = ["https://install.determinate.systems"];
    trusted-public-keys = ["cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="];
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
      # how many terminals does a guy need?
      kitty.enable = true;
      wezterm.enable = true;
      _1password.enable = true;
      git = {
        enable = true;
        useDelta = true;
        useJJ = true;
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
      age = {
        enable = true;
        secretsDir = ./secrets;
      };
    };
    services = {
      zfs.enable = true;
      docker.enable = true;
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
      # NOTE this will break on flake upgrade
      # https://nixpk.gs/pr-tracker.html?pr=432535
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
      # environment.sway.enable = true;
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
        openvpn
        papers
        signal-desktop-bin
        spotify
        ungoogled-chromium
      ];
    };
    themes.active = "tokyo-night-dark";
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_6_16;
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

  # apparently github will rate-limit your access to the API
  # so this is a workaround for that
  # nix.settings.access-tokens = ''
  #   !include ${config.age.secrets.github.path}
  # '';

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
