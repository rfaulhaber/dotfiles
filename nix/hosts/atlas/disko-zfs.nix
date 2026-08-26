# Install-time disko spec for atlas's OS disks. Not imported by the flake:
# runtime mounts come from hardware-configuration.nix and the dataset layer
# from modules.services.zfs.datasets / the OCI modules. Run it only from an
# installer, against the two system-pool devices below.
#
# The data pool is deliberately absent so disko can never format it. A
# reinstall imports the existing pool instead (`zpool import -f data`;
# boot-time import comes from boot.zfs.extraPools in configuration.nix).
# Reference, as built:
#
#   data (raidz2 x3, 20 SAS disks on the LSI HBA, ashift=12, ~188T raw)
#     raidz2-0: 6x 12TB HGST HUH721212AL4200
#     raidz2-1: 6x 12TB HGST HUH721212AL4200
#     raidz2-2: 8x  8TB (6x H7280A520SUN8.0T + 2x HUH728080AL52xx)
#   No log/cache/spare vdevs. The SAS disks expose only wwn- by-id names.
#   Pool-root props inherited by every dataset: mountpoint=none,
#   recordsize=1M, compression=lz4, atime=off, xattr=sa, acltype=posix.
#
# Most data-pool datasets are declared in modules.services.zfs.datasets and
# recreated/adopted by zfs-manage-datasets on first boot. Hand-created ones
# with no declaration anywhere in the repo (recreate manually after a
# from-scratch rebuild): data/downloads/{nzb,slskd,transmission},
# data/files/archive{,/tumblr}, data/files/{org,sync} (encrypted, sops-held
# keys), data/games/{exo/{3x,9x,dos},roms}, data/music, data/tv/gundam.
{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        # 128GB Kingston SATA SSD; also carries the ESP.
        device = "/dev/disk/by-id/ata-KINGSTON_RBU-SC100S37128GD_50026B725301372C";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = ["umask=0077"];
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "system";
              };
            };
          };
        };
      };
      nvme = {
        type = "disk";
        # 512GB Team Group NVMe.
        device = "/dev/disk/by-id/nvme-TEAM_TM8FP4512G_TPBF2108200040901628";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "system";
              };
            };
          };
        };
      };
    };
    zpool = {
      # Two mismatched single-disk top-level vdevs: a plain stripe with no
      # redundancy. Losing either device loses the pool.
      system = {
        type = "zpool";
        options.ashift = "12";
        rootFsOptions = {
          mountpoint = "none";
          atime = "off";
          xattr = "sa";
          acltype = "posix";
        };
        datasets = {
          "root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options.mountpoint = "legacy";
          };
          "nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options.mountpoint = "legacy";
          };
          "home" = {
            type = "zfs_fs";
            mountpoint = "/home";
            options.mountpoint = "legacy";
          };
          "var" = {
            type = "zfs_fs";
            mountpoint = "/var";
            options.mountpoint = "legacy";
          };
        };
      };
    };
  };
}
