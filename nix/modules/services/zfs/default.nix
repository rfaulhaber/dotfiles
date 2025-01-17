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
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.boot.kernelPackages.zfs.meta.broken == false;
        message = "ZFS is marked as broken in this kernel package. Please address.";
      }
    ];
    services.zfs = {
      autoScrub.enable = true;
      trim.enable = true;
      autoSnapshot.enable = true;
    };
  };
}
