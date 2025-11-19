{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.zfs;
  mkFilesystemFromDataset = name: mountpoint: {
    ${mountpoint} = {
      device = name;
      fsType = "zfs";
      options = ["zfsutil"];
    };
  };
  zfsExec = "${pkgs.zfs}/bin/zfs";

  mkPropertyCmd = name: value: "${zfsExec} list ${dataset} > /dev/null 2>&1 || ${zfsExec} set ${propertyName}=${value} ${dataset}";
  mkSystemdService = dataset: properties: {
    "zfs-create-${dataset}" = {
      description = "Creates ${dataset} ZFS dataset";
      wantedBy = ["multi-user.target"];
      after = ["zfs-import.target"];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script =
        ''
          ${zfsExec} list ${dataset} > /dev/null 2>&1 || zfs create ${dataset}
        ''
        + properties
        |> mapAttrs mkPropertyCmd
        |> concatStringsSep "\n";
    };
  };
in {
  options.modules.services.zfs = {
    enable = mkEnableOption false;
    shares = mkOption {
      description = "ZFS shares.";
      type = types.attrsOf (types.submodule {
        options = {
          path = mkOption {
            description = "Dataset to share.";
            type = types.str;
            example = "data/something_to_share";
          };
        };
      });
    };
    datasets = mkOption {
      description = "Declarative ZFS datasets.";
      type = types.attrsOf (types.submodule {
        options = {
          mountpoint = mkOption {
            description = "Where to mount the dataset.";
            type = types.either types.str types.path;
          };
          datasetOptions = mkOption {
            description = "ZFS dataset options.";
            type = types.attrsOf types.str;
            example = {
              "mountpoint" = "/mnt/dataset";
              "encryption" = "on";
            };
            default = {};
          };
        };
      });
    };
    # NOTE in order for this to work properly, see the following:
    # - https://wiki.archlinux.org/title/ZFS (section 6.1.2)
    # - https://www.reddit.com/r/NixOS/comments/tzksw4/comment/i4dw7f8
    encryptedHome = mkOption {
      description = "Home dataset to be decrypted on boot.";
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.boot.kernelPackages.${pkgs.zfs.kernelModuleAttribute}.meta.broken == false;
        message = "ZFS is marked as broken in this kernel package. Please address.";
      }
    ];

    services.zfs = {
      autoScrub.enable = true;
      trim.enable = true;
      autoSnapshot.enable = true;
    };

    # TODO throw in some assertions to check this?
    security.pam.zfs = mkIf (cfg.encryptedHome != null) {
      enable = true;
      homes = cfg.encryptedHome;
    };

    # TODO need way to override for multiple encrypted datasets
    boot.zfs.requestEncryptionCredentials = cfg.encryptedHome == null;
  };
}
