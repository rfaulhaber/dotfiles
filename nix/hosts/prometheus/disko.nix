# Minimal disko layout for a Raspberry Pi 5 target, suitable for
# nixos-anywhere. Two partitions on a single device:
#   1. FIRMWARE — FAT32, holds the Pi bootloader, kernels, initrds, DTBs.
#      Sized at 1 GiB to match the upstream nixos-raspberrypi sd-image
#      (firmwareSize = 1024) so the "kernel" generational bootloader has
#      room for many NixOS generations.
#   2. root    — ext4, the rest of the disk.
#
# Override the device at build/deploy time, e.g.:
#   nixos-anywhere --flake .#prometheus \
#     --disk-encryption-keys /tmp/ignored \
#     --disko-mode destroy,format,mount \
#     --extra-files /tmp/extras \
#     --target-host root@<ip>
# or set disko.devices.disk.main.device in a host override.
{lib, ...}: {
  disko.devices = {
    disk.main = {
      device = lib.mkDefault "/dev/mmcblk0";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          firmware = {
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot/firmware";
              mountOptions = ["noatime"];
            };
          };
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
              mountOptions = ["noatime"];
            };
          };
        };
      };
    };
  };
}
