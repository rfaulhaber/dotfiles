{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.nfs.mount;
  mkMount = _name: mountCfg: {
    device = "${mountCfg.server}:${mountCfg.path}";
    fsType = "nfs";
    options = mountCfg.fsOptions;
  };
in {
  options.modules.services.nfs.mount = {
    enable = mkEnableOption "NFS client mounts";

    mounts = mkOption {
      description = "NFS mounts to configure. Attribute names are used as mount paths (e.g. /mnt/movies).";
      type = types.attrsOf (types.submodule {
        options = {
          server = mkOption {
            description = "NFS server address (IP or hostname).";
            type = types.str;
            example = "192.168.86.63";
          };
          path = mkOption {
            description = "Remote export path on the NFS server.";
            type = types.str;
            example = "/data/movies";
          };
          fsOptions = mkOption {
            description = "Mount options for the NFS filesystem.";
            type = types.listOf types.str;
            default = [
              "x-systemd.automount"
              "noauto"
              "x-systemd.idle-timeout=60"
              "x-systemd.device-timeout=5s"
              "x-systemd.mount-timeout=5s"
              "nfsvers=4"
            ];
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.nfs-utils];
    fileSystems = mapAttrs mkMount cfg.mounts;
  };
}
