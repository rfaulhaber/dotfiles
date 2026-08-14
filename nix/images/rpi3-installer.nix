# Minimal SD card image for bootstrapping a Raspberry Pi 3B.
# Build with: nix build .#rpi3-installer
# Flash with: zstd -d result/sd-image/*.img.zst -o hecate.img && dd if=hecate.img of=/dev/sdX bs=4M status=progress
{
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
    hostName = "rpi3b";
    useDHCP = true;
  };

  services.openssh = {
    enable = true;
    # initialPassword below is public in this repo — keep it console-only.
    # SSH access is via the baked-in authorized keys.
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

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
