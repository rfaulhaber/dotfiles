{
  config,
  lib,
  pkgs,
  inputs,
  nixos-raspberrypi,
  ...
}: {
  imports = [
    ./hardware.nix
    ../../modules
    # Required when using nixpkgs.lib.nixosSystem directly with the Pi 5 modules:
    # applies the vendor kernel, firmware, and bootloader overlays.
    nixos-raspberrypi.lib.inject-overlays
    nixos-raspberrypi.nixosModules.raspberry-pi-5.base
    nixos-raspberrypi.nixosModules.raspberry-pi-5.page-size-16k
    nixos-raspberrypi.nixosModules.trusted-nix-caches
    # Provides system.build.sdImage and the firmware-partition wiring; also
    # selects the "kernel" generational bootloader for Pi 5 automatically.
    nixos-raspberrypi.nixosModules.sd-image
    inputs.determinate.nixosModules.default
  ];

  nix.settings = {
    substituters = [
      "https://install.determinate.systems"
      "https://nixos-raspberrypi.cachix.org"
      "http://vulcan.lan:4965"
    ];
    trusted-public-keys = [
      "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
      "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
    ];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      sops = {
        enable = true;
        keyFile = null;
        secrets = {
          "forgejo-runner/token" = {};
          "codeberg-runner/token" = {};
        };
      };
    };
    services = {
      sudo-rs.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 13571;
        };
      };
      netbird.enable = true;
    };

    linux.oci = {
      enable = true;
      services.forgejo-runner = {
        enable = true;
        runners = {
          # Native aarch64 builder for the CI matrix. Hosts whose `runs-on`
          # matches the `nix-aarch64` label land here instead of going through
          # vulcan's binfmt emulation; for prometheus itself this also primes
          # the local /nix/store so nixos-rebuild on the host is a cache hit.
          default = {
            enable = true;
            instanceUrl = "http://git.home.lan";
            capacity = 2;
            jobStateDir = "/apps/forgejo-runner/default-state";
            tokenFile = config.sops.templates."forgejo-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix-aarch64:docker://nixos/nix:latest"
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
            capacity = 2;
            tokenFile = config.sops.templates."codeberg-runner-env".path;
            labels = [
              "docker:docker://node:20-bookworm"
              "ubuntu-latest:docker://ubuntu:latest"
              "nix-aarch64:docker://nixos/nix:latest"
            ];
            jobStateDir = "/apps/forgejo-runner/codeberg-state";
            baseDir = "/apps/forgejo-runner/codeberg";
            validVolumes = [
              "/nix/var/nix/daemon-socket/socket"
            ];
            containerOptions = "-v /nix/var/nix/daemon-socket/socket:/nix/var/nix/daemon-socket/socket";
          };
        };
      };
    };

    themes.active = "moonlight";
  };

  sops.templates = {
    "forgejo-runner-env".content = ''
      FORGEJO_TOKEN=${config.sops.placeholder."forgejo-runner/token"}
    '';
    "codeberg-runner-env".content = ''
      FORGEJO_TOKEN=${config.sops.placeholder."codeberg-runner/token"}
    '';
  };

  hardware.enableRedistributableFirmware = true;

  # Pi 5's 16K-page-size aarch64 kernel rejects NixOS's default of 33 because
  # ARCH_MMAP_RND_BITS_MAX is lower for 16K pages. 18 sits safely below it.
  boot.kernel.sysctl."vm.mmap_rnd_bits" = 18;

  console.enable = false;

  # The nixos-raspberrypi cachix has the vendor kernel but not its `-dev` output,
  # which ZFS would need; opting out keeps the build a pure cache fetch.
  boot.supportedFilesystems.zfs = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  networking = {
    useDHCP = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [];
      allowedUDPPorts = [];
    };
  };

  system.nixos.tags = let
    cfg = config.boot.loader.raspberry-pi;
  in [
    "raspberry-pi-${cfg.variant}"
    cfg.bootloader
    config.boot.kernelPackages.kernel.version
  ];

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;
}
