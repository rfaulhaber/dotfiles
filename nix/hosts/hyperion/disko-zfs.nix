{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/nvme1n1";
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
      zpool = {
        zroot = {
          type = "zpool";
          mountpoint = "none";
          rootFsOptions = {
            ashift = 12;
            atime = "off";
            xattr = "sa";
          };
          datasets = {
            "nix" = {
              type = "zfs_fs";
              mountpoint = "/nix";
              options = {
                compression = "lz4";
                mountpoint = "legacy";
              };
            };
            "root" = {
              type = "zfs_fs";
              mountpoint = "/root";
              options.mountpoint = "legacy";
            };
            "home" = {
              type = "zfs_fs";
              mountpoint = "/home";
              options.mountpoint = "legacy";
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "file:///tmp/secret.key";
              postCreateHook = ''
                zfs set keylocation="prompt" "zroot/home";
              '';
            };

            "home/cache" = {
              type = "zfs_fs";
              mountpoint = "/home/.cache";
              options = {
                mountpoint = "legacy";
                compression = "lz4";
              };
            };
          };
        };
      };
    };
  };
}
