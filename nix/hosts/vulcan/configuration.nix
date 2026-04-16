{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.disko.nixosModules.disko
    ./disko.nix
    ./hardware.nix
    inputs.determinate.nixosModules.default
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  nix.settings = {
    substituters = ["https://install.determinate.systems"];
    trusted-public-keys = ["cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        zoxide.enable = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {
          nix-cache = {};
        };
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
      nix-cache = {
        enable = true;
        port = 4965;
        interface = "enp4s0";
        secretKeyFile = config.sops.secrets.nix-cache.path;
      };
      nfs.mount = {
        enable = true;
        mounts = {
          "/mnt/media/movies" = {
            server = "atlas";
            path = "/data/movies";
          };
          "/mnt/media/tv" = {
            server = "atlas";
            path = "/data/tv";
          };
        };
      };
    };

    hardware.intel-gpu.enable = true;

    linux.oci = {
      enable = true;
      zfs = {
        enable = true;
        pool = "zroot";
      };
      services = {
        plex = {
          enable = true;
          baseDir = "/apps/plex";
          gpu = "intel";
          openFirewall = true;
          mediaDirs = {
            movies = "/mnt/media/movies";
            tv = "/mnt/media/tv";
          };
        };
        jellyfin = {
          enable = true;
          baseDir = "/apps/jellyfin";
          gpu = "intel";
          openFirewall = true;
          tvDir = "/mnt/media/tv";
          moviesDir = "/mnt/media/movies";
        };
        immich-ml = {
          enable = true;
          gpu = "intel";
          openFirewall = true;
        };
        newt = {
          enable = true;
          pangolinEndpoint = "https://pangolin.3679.space";
          secretsFile = config.sops.templates."newt-env".path;
          dns = "192.168.0.2";
        };
        forgejo-runner = {
          enable = true;
          runners = {
            default = {
              enable = true;
              # vulcan and atlas are on the same LAN
              instanceUrl = "http://git.home.lan";
              # Allow up to 4 concurrent jobs so CI workflows that use
              # strategy.matrix (e.g. per-host NixOS builds) can run in parallel.
              capacity = 4;
              tokenFile = config.sops.templates."forgejo-runner-env".path;
              labels = [
                "docker:docker://node:20-bookworm"
                "ubuntu-latest:docker://ubuntu:latest"
                "nix:docker://nixos/nix:latest"
              ];
              baseDir = "/apps/forgejo-runner/default";
              validVolumes = [
                "/nix/var/nix/daemon-socket/socket"
              ];
              containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
            };
            codeberg = {
              enable = true;
              instanceUrl = "https://codeberg.org";
              capacity = 4;
              tokenFile = config.sops.templates."codeberg-runner-env".path;
              labels = [
                "docker:docker://node:20-bookworm"
                "ubuntu-latest:docker://ubuntu:latest"
                "nix:docker://nixos/nix:latest"
              ];
              baseDir = "/apps/forgejo-runner/codeberg";
              validVolumes = [
                "/nix/var/nix/daemon-socket/socket"
              ];
              containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
            };
          };
        };
      };
    };

    themes.active = "tokyo-night-dark";
  };

  boot = {
    binfmt.emulatedSystems = ["aarch64-linux"];
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

  sops = {
    secrets = {
      "newt/id" = {};
      "newt/secret" = {};
      "forgejo-runner/token" = {};
      "codeberg-runner/token" = {};
    };
    templates = {
      "forgejo-runner-env".content = ''
        FORGEJO_TOKEN=${config.sops.placeholder."forgejo-runner/token"}
      '';
      "codeberg-runner-env".content = ''
        FORGEJO_TOKEN=${config.sops.placeholder."codeberg-runner/token"}
      '';
      "newt-env".content = ''
        NEWT_ID=${config.sops.placeholder."newt/id"}
        NEWT_SECRET=${config.sops.placeholder."newt/secret"}
      '';
    };
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
