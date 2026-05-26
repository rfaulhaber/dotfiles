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
          "codeberg-runner/token" = {};
          nix-cache = {};
          "netbird/setup-key" = {};
        };
      };
    };
    services = {
      sudo-rs.enable = true;
      observability-agent = {
        enable = true;
        prometheus.openFirewall = true;
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
