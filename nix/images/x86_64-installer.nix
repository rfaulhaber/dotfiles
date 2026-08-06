# Minimal x86_64 installer ISO with ZFS support.
# Build with: nix build .#x86_64-installer
# Flash with: dd if=result/iso/*.iso of=/dev/sdX bs=4M status=progress
{
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
  ];

  boot.supportedFilesystems = ["zfs"];
  networking.hostId = "00000000";

  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

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

  system.stateVersion = "26.05";
}
