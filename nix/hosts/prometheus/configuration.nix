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
    ../../modules/hardware/rpi5-vendor-kernel.nix
    ./oci.nix
    # Required when using nixpkgs.lib.nixosSystem directly with the Pi 5 modules:
    # applies the vendor kernel, firmware, and bootloader overlays.
    nixos-raspberrypi.lib.inject-overlays
    nixos-raspberrypi.nixosModules.raspberry-pi-5.base
    nixos-raspberrypi.nixosModules.trusted-nix-caches
    # Provides system.build.sdImage and the firmware-partition wiring; also
    # selects the "kernel" generational bootloader for Pi 5 automatically.
    nixos-raspberrypi.nixosModules.sd-image
    inputs.determinate.nixosModules.default
  ];

  modules = {
    # The aarch64 CI runners build through this host's nix daemon and
    # inherit its substituters (the old configure-nix.nu composed this
    # list per-job).
    nix.substituters.enable = true;
    programs = {
      btop.enable = true;
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
          "github-runner/token" = {};
          nix-cache = {};
          "netbird/setup-key" = {};
        };
      };
    };
    services = {
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        prometheus = {
          openFirewall = true;
          interface = "end0";
        };
        loki.extraLabels.role = "ci-aarch64";
      };
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 13571;
        };
      };
      netbird = {
        enable = true;
        setupKeyFile = config.sops.secrets."netbird/setup-key".path;
      };
      # Serve aarch64 builds back over the LAN so future CI runs and
      # nixos-rebuilds on this host hit local cache instead of recompiling
      # against cache.nixos.org / cachix. Port matches vulcan's harmonia
      # for consistency. Public key needs to be added to the workflow's
      # extra-trusted-public-keys before clients will trust paths from here.
      nix-cache = {
        enable = true;
        port = 4965;
        interface = "end0";
        secretKeyFile = config.sops.secrets.nix-cache.path;
      };
      github-runner = {
        enable = true;
        url = "https://github.com/rfaulhaber/dotfiles";
        tokenFile = config.sops.secrets."github-runner/token".path;
        # Two concurrent jobs is what the Pi 5 sustained under the Forgejo
        # runner; the third aarch64 matrix leg queues behind them.
        count = 2;
        extraLabels = ["nix-aarch64"];
      };
    };

    themes.active = "moonlight";
  };

  sops.templates = {
    "forgejo-runner-env".content = ''
      FORGEJO_TOKEN=${config.sops.placeholder."forgejo-runner/token"}
    '';
  };

  hardware.enableRedistributableFirmware = true;

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
}
