{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.samba.mount;
  mkMount = name: mountCfg: {
    device = "//${mountCfg.server}/${mountCfg.share}";
    fsType = "cifs";
    options =
      mountCfg.fsOptions
      ++ [
        "credentials=${mountCfg.credentials}"
        "uid=${toString mountCfg.uid}"
        "gid=${toString mountCfg.gid}"
      ];
  };
in {
  options.modules.services.samba.mount = {
    enable = mkEnableOption "Samba CIFS mounts";
    mounts = mkOption {
      description = "CIFS mounts to configure. Attribute names are used as mount paths (e.g. /mnt/share).";
      type = types.attrsOf (types.submodule {
        options = {
          server = mkOption {
            type = types.str;
            description = "Address of the Samba server (IP or hostname).";
            example = "192.168.86.63";
          };
          share = mkOption {
            type = types.str;
            description = "Name of the Samba share on the server.";
            example = "calibre";
          };
          credentials = mkOption {
            type = types.str;
            description = "Path to a Samba credentials file. Recommended: use a sops-nix secret path.";
            example = ''config.sops.secrets."smb-credentials".path'';
          };
          uid = mkOption {
            type = types.ints.positive;
            description = "UID for mount point ownership.";
            default = config.user.uid;
            example = 1000;
          };
          gid = mkOption {
            type = types.ints.positive;
            description = "GID for mount point ownership.";
            example = 100;
          };
          fsOptions = mkOption {
            type = types.listOf types.str;
            description = "Mount options for the CIFS filesystem.";
            default = [
              "x-systemd.automount"
              "noauto"
              "x-systemd.idle-timeout=60"
              "x-systemd.device-timeout=5s"
              "x-systemd.mount-timeout=5s"
            ];
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.cifs-utils];
    fileSystems = mapAttrs mkMount cfg.mounts;
  };
}
