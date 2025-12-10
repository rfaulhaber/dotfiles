{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.zfs;
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
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          type = mkOption {
            type = types.enum ["filesystem" "volume"];
            description = "Type of ZFS dataset to create";
            default = "filesystem";
          };
          properties = mkOption {
            description = "ZFS dataset options.";
            type = types.attrsOf types.str;
            default = {};
            example = {
              "mountpoint" = "/mnt/dataset";
              "encryption" = "on";
            };
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

    systemd.services.zfs-manage-datasets = mkIf (cfg.datasets
      != {}) {
      description = "Create ZFS datasets.";
      wantedBy = ["multi-user.target"];
      after = ["zfs-import.target" "zfs-mount.service"];
      before = ["local-fs.target"];
      path = with pkgs; [nushell zfs];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = let
          zfsManageScript =
            builtins.readFile "${config.dotfiles.binDir}/zfs-manage.nu"
            |> lib.my.writeNushellScriptBin "zfs-manage";
          datasetsJSON = builtins.toJSON cfg.datasets;
        in "${zfsManageScript}/bin/zfs-manage '${datasetsJSON}'";
      };
    };
  };
}
