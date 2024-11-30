let
  lib = (import <nixpkgs> {}).lib;
  mkDevice = pool: name: {
    ${name} = {
      type = "disk";
      device = "/dev/${name}";
      content = {
        type = "gpt";
        partitions = {
          zfs = {
            size = "100%";
            content = {
              inherit pool;
              type = "zfs";
            };
          };
        };
      };
    };
  };
  mkDataDevice = mkDevice "data";
  devices8tb = [];
  nvme = "";
  ssd = "";
  disks = lib.pipe devices8tb [
    (builtins.map mkDataDevice)
    (builtins.foldl' (acc: val: acc // val) {})
  ];
in {
  disko.devices = {
    disk =
      {
        main = {
          type = "disk";
          device = "/dev/sda";
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
          device = "/dev/nvme0n1";
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
      }
      // disks;
    zpool = {
      system = {
        type = "zpool";
        mode = {
          topology = {
            vdev = [
              {
                members = [
                  "/dev/sda"
                  "/dev/nvme0n1"
                ];
              }
            ];
          };
        };
        rootFsOptions = {
          atime = "off";
          xattr = "sa";
        };
        options.ashift = "12";
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
          "var" = {
            type = "zfs_fs";
            mountpoint = "/var";
            options = {
              compression = "lz4";
              mountpoint = "legacy";
            };
          };
          "config" = {
            type = "zfs_fs";
            mountpoint = "/config";
            options = {
              compression = "lz4";
              mountpoint = "legacy";
            };
          };
        };
      };
      data = {
        type = "zpool";
        mode = {
          topology = {
            type = "topology";
            vdev = [
              {
                members = builtins.map (device: "/dev/${device}") devices12tb;
                mode = "raidz2";
              }
            ];
            log = [
              {
                members = [logSSD];
              }
            ];
          };
        };
        rootFsOptions = {
          atime = "off";
          xattr = "sa";
        };
        options.ashift = "12";
        datasets = {
          "data" = {
            type = "zfs_fs";
            mountpoint = "/data";
            options.mountpoint = "legacy";
          };
          "movies" = {
            type = "zfs_fs";
            mountpoint = "/data/movies";
            options.mountpoint = "legacy";
          };
          "tv" = {
            type = "zfs_fs";
            mountpoint = "/data/tv";
            options.mountpoint = "legacy";
          };
          "books" = {
            type = "zfs_fs";
            mountpoint = "/data/books";
            options.mountpoint = "legacy";
          };
          "backup" = {
            type = "zfs_fs";
            mountpoint = "/data/backup";
            options.mountpoint = "legacy";
          };
        };
      };
    };
  };
}
