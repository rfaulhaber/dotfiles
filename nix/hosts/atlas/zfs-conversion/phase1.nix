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
  devices12tb = ["sda"];
  logSSD = "";
  cacheSSD = "";
  disks = lib.pipe devices12tb [
    (builtins.map mkDataDevice)
    (builtins.foldl' (acc: val: acc // val) {})
  ];
in {
  disko.devices = {
    disk = disks;
    zpool = {
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
