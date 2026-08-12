# Minimal SD card image for bootstrapping a Raspberry Pi 5.
# Build with: nix build .#rpi5-installer
# Flash with: zstd -d result/sd-image/*.img.zst -o rpi5.img && dd if=rpi5.img of=/dev/sdX bs=4M status=progress
{
  inputs,
  nixos-raspberrypi,
  ...
}: {
  imports = [
    # Shared with nix/hosts/prometheus: grafts target/buildDTBs onto the vendor
    # kernel's passthru, which nixpkgs >= 26.11 requires.
    ../modules/hardware/rpi5-vendor-kernel.nix
    # Vendor kernel/firmware/bootloader overlays applied to the local pkgs set.
    nixos-raspberrypi.lib.inject-overlays
    nixos-raspberrypi.nixosModules.raspberry-pi-5.base
    nixos-raspberrypi.nixosModules.raspberry-pi-5.page-size-16k
    # Provides sdImage build + /boot/firmware wiring, and picks the "kernel"
    # generational bootloader for Pi 5 automatically.
    nixos-raspberrypi.nixosModules.sd-image
    nixos-raspberrypi.nixosModules.trusted-nix-caches
    inputs.determinate.nixosModules.default
  ];

  hardware.enableRedistributableFirmware = true;

  # The nixos-raspberrypi cachix hosts the prebuilt vendor kernel but NOT its
  # `-dev` output. ZFS (pulled in by default via profiles/base.nix) compiles
  # against `-dev`, which forces a full kernel rebuild. The installer has no
  # need for ZFS, so opt out to keep the build as a pure cache fetch.
  boot.supportedFilesystems.zfs = false;

  nixpkgs.config.allowUnfree = true;

  networking = {
    hostName = "rpi5";
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

  users.users.root.openssh.authorizedKeys.keys = import ../modules/ssh/keys.nix;

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
