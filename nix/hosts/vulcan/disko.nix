{
  disko.devices = {
    # Disks are pinned by-id rather than /dev/nvmeXn1: with two NVMe devices
    # the kernel's enumeration order is not tied to the physical M.2 slot, so
    # firmware or kernel changes can renumber them. Nothing the running system
    # depends on is slot-bound either — the ESP is found by partlabel and the
    # datasets by pool — so the drives may be moved between slots freely.
    disk = {
      # M2_2 (PCIe Gen3 x2, chipset-attached). The OS has no use for the
      # bandwidth of the CPU-attached slot, so it yields M2_1 to the games pool.
      main = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-WD_PC_SN740_SDDQNQD-256G-2006_24010E806964"; # WD PC SN740 256GB
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
                pool = "zroot";
              };
            };
          };
        };
      };

      # M2_1 (Hyper M.2, PCIe Gen4 x4, CPU-attached).
      # TODO: replace with the real by-id path once the drive is installed:
      #   ls /dev/disk/by-id/ | find nvme | find -v part
      store = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-WD_BLACK_SN7100_2TB_254637801528";
        content = {
          type = "gpt";
          partitions = {
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "store";
              };
            };
          };
        };
      };
    };

    zpool = {
      zroot = {
        type = "zpool";
        rootFsOptions = {
          atime = "off";
          xattr = "sa";
        };
        options.ashift = "12";
        # The live datasets carry mountpoint=legacy; without declaring that
        # here, disko appends zfsutil to the generated fstab entries, which
        # only works against native-mountpoint datasets.
        datasets = {
          "root" = {
            type = "zfs_fs";
            mountpoint = "/";
            options.mountpoint = "legacy";
          };
          "nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options = {
              compression = "lz4";
              mountpoint = "legacy";
            };
          };
          "home" = {
            type = "zfs_fs";
            mountpoint = "/home";
            options.mountpoint = "legacy";
          };
        };
      };

      # A separate pool rather than a second vdev in zroot: ZFS would stripe
      # across both devices, so losing either would take the root filesystem
      # with it. Isolated, this pool holds only re-acquirable data and can be
      # destroyed and rebuilt without touching the OS.
      #
      # Datasets inherit their mountpoints from the pool, landing under
      # /store/games/{steam,state}.
      store = {
        type = "zpool";
        options.ashift = "12";
        rootFsOptions = {
          atime = "off";
          xattr = "sa";
          compression = "lz4";
          # services.zfs.autoSnapshot is enabled host-wide by the zfs module.
          # Left on, every dataset here would accumulate snapshots of content
          # Steam can re-download; the same setting has 75G pinned across the
          # Plex and Jellyfin datasets in zroot.
          "com.sun:auto-snapshot" = "false";
        };
        datasets = {
          "games" = {
            type = "zfs_fs";
          };
          # Game content is large files read sequentially, so a big record cuts
          # per-block overhead. Matches data/games on atlas.
          "games/steam" = {
            type = "zfs_fs";
            options.recordsize = "1M";
          };
          # Proton prefixes and shader caches are thousands of small files
          # rewritten during play. Split out so the 1M record above never
          # applies to them, and so the library can be wiped independently of
          # per-game state.
          "games/state" = {
            type = "zfs_fs";
          };
        };
      };
    };
  };
}
