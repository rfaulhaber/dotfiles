{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ../../modules
    inputs.disko.nixosModules.disko
    ./disko.nix
    ./hardware.nix
    inputs.determinate.nixosModules.default
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  nix.settings = {
    substituters = [
      "https://install.determinate.systems"
      "http://atlas.lan:4965"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "atlas.lan-1:ElfYJ8gkV4CN7S1afAl/Y3lfXYB5P6K7wf+XC+rBUIs="
    ];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
    };
    services = {
      zfs.enable = true;
      sudo-rs.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 13308;
        };
      };
      netbird.enable = true;
      # nfs.mount = {
      #   enable = true;
      #   mounts = {
      #     "/mnt/media/movies" = {
      #       server = "atlas";
      #       path = "/data/movies";
      #     };
      #     "/mnt/media/tv" = {
      #       server = "atlas";
      #       path = "/data/tv";
      #     };
      #   };
      # };
    };

    hardware.intel-gpu.enable = true;

    # linux.oci = {
    #   enable = true;
    #   services = {
    #     plex = {
    #       enable = true;
    #       baseDir = "/data/apps/plex";
    #       gpu = "intel";
    #       openFirewall = true;
    #       mediaDirs = {
    #         movies = "/mnt/media/movies";
    #         tv = "/mnt/media/tv";
    #       };
    #     };
    #     jellyfin = {
    #       enable = true;
    #       baseDir = "/data/apps/jellyfin";
    #       gpu = "intel";
    #       openFirewall = true;
    #       mediaDirs = {
    #         movies = "/mnt/media/movies";
    #         tv = "/mnt/media/tv";
    #       };
    #     };
    #     immich-ml = {
    #       enable = true;
    #       gpu = "intel";
    #       openFirewall = true;
    #     };
    #   };
    # };

    themes.active = "tokyo-night-dark";
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages;
    kernelParams = ["nohibernate"];
    zfs.extraPools = ["zroot"];
  };

  networking = {
    hostName = "vulcan";
    hostId = "896980a5";

    useDHCP = false;
    interfaces.enp4s0.useDHCP = true;

    firewall.enable = true;
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
