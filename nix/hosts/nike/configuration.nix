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
  imports = [
    ./hardware-configuration.nix
    ../../modules
    inputs.nixos-hardware.nixosModules.raspberry-pi-5
    (lib.mkAliasOptionModuleMD ["environment" "checkConfigurationOptions"] ["_module" "check"])
    # inputs.nixos-raspberrypi.lib.inject-overlays
    # inputs.nixos-raspberrypi.nixosModules.sd-image
    # inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.base
    # inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.display-vc4
    # inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5.bluetooth
  ];

  disabledModules = [
    (modulesPath + "/rename.nix")
  ];

  # TODO place elsewhere
  nix.settings = {
    extra-substituters = [
      "https://nixos-raspberrypi.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

  modules = {
    # desktop = {
    #   enable = true;
    #   # retropie-nix.enable = true;
    # };
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
          keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHlCcs8h2PrT3GcOVs6K0IGozqV8yuR945ZDr8eYhqfj ryan@hyperion"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID9EVRAxaCrK68NSCoiNjjQLqu4k13Z45tCBb0jGAtC/ ryan@eos"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPhN+t0aI3pQhZsFPoRn8dWe7YvDn3ehWOUmwvqbQyZP ryan@1p"
          ];
          port = 14625;
        };
      };
      samba-mount = {
        enable = true;
        mounts."${config.user.home}/games" = {
          domain = "192.168.0.3";
          host = "games";
          secrets = "/etc/nixos/smb-secrets";
        };
      };
    };

    themes.active = "moonlight";
  };

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

  boot.loader = {
    grub.enable = false;
    generic-extlinux-compatible.enable = true;
  };

  # raspberry pi hardware configuration
  # hardware = {
  #   raspberry-pi."5" = {
  #     fkms-3d.enable = true;
  #     apply-overlays-dtmerge.enable = true;
  #   };

  #   enableRedistributableFirmware = true;
  # };

  console.enable = false;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  # temporary, make nix settings modular
  nix.gc.automatic = lib.mkForce false;

  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
