# Minimal SD card image for bootstrapping a Raspberry Pi 3B.
# Build with: nix build .#hecate-installer
# Flash with: zstd -d result/sd-image/*.img.zst -o hecate.img && dd if=hecate.img of=/dev/sdX bs=4M status=progress
{
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}: {
  imports = [
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
    inputs.nixos-hardware.nixosModules.raspberry-pi-3
  ];

  boot.kernelPackages = pkgs.linuxPackages;

  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  networking = {
    hostName = "hecate";
    useDHCP = false;
    interfaces.eth0.ipv4.addresses = [
      {
        address = "192.168.0.43";
        prefixLength = 24;
      }
    ];
    defaultGateway = "192.168.0.1";
    nameservers = ["192.168.0.2" "1.1.1.1"];
  };

  services.openssh.enable = true;

  users.users.ryan = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys = import ../modules/ssh/keys.nix;
    initialPassword = "nixos";
  };

  nix.settings = {
    experimental-features = ["nix-command" "flakes" "pipe-operators" "ca-derivations"];
    trusted-users = ["ryan"];
    allowed-users = ["ryan"];
  };

  security.pam = {
    sshAgentAuth.enable = true;
    services.ryan.sshAgentAuth = true;
  };

  system.stateVersion = "25.05";
}
