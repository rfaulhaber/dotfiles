# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  inputs,
  modulesPath,
  ...
}: {
  imports =
    [
      ./hardware-configuration.nix
    ]
    ++ (with inputs.nixos-raspberrypi.nixosModules; [
      raspberry-pi-5.base
      raspberry-pi-5.page-size-16k
      raspberry-pi-5.display-vc4
      # usb-gadget-ethernet
      # inputs.nixos-raspberrypi.lib.inject-overlays
      # trusted-nix-caches
      # # nixpkgs-rpi
      # inputs.nixos-raspberrypi.lib.inject-overlays-global
    ]);

  disabledModules = [
    (modulesPath + "/rename.nix")
  ];

  nix.settings = {
    extra-substituters = [
      "https://nixos-raspberrypi.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

  modules = {
    programs = {
      nushell = {
        enable = true;
        setDefault = true;
        carapace.enable = true;
      };
      neovim.enable = true;
      git.enable = true;
    };
    services = {
      gpg.enable = true;
      ssh = {
        enable = true;
        server = {
          enable = true;
          port = 14625;
        };
      };
    };

    themes.active = "moonlight";
  };

  system.nixos.tags = let
    cfg = config.boot.loader.raspberryPi;
  in [
    "raspberry-pi-${cfg.variant}"
    cfg.bootloader
    config.boot.kernelPackages.kernel.version
  ];

  networking = {
    hostName = "nike";
    hostId = "51F49153";
    # useDHCP = true;
    interfaces.end0.useDHCP = true;

    dhcpcd.extraConfig = ''
      blacklist 192.168.0.1
    '';

    firewall.enable = true;
  };

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
