{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.services.samba-mount;
  mkMount = name:
    { fsOptions, domain, host, secrets, uid, gid }: {
      device = "//${domain}/${host}";
      fsType = "cifs";
      options = fsOptions ++ [
        "credentials=${secrets}"
        "uid=${toString uid}"
        "gid=${toString gid}"
      ];
    };
in {
  options.modules.services.samba-mount = {
    enable = mkEnableOption false;
    mounts = mkOption {
      description = "List of mounts.";
      type = types.attrsOf (types.submodule {
        options = {
          domain = mkOption {
            type = types.str;
            description = "Address of Samba mount.";
            example = "192.168.86.63";
          };
          host = mkOption {
            type = types.str;
            description = "Samba host directory.";
            example = "calibre";
          };
          secrets = mkOption {
            type = types.either types.path types.str;
            description = "Path to Samba secrets file.";
            default = "/etc/nixos/smb-secrets";
            example = "/etc/nixos/smb-secrets";
          };
          uid = mkOption {
            type = types.ints.positive;
            description = "UID for mount point.";
            default = config.user.uid;
            example = 1000;
          };
          gid = mkOption {
            type = types.ints.positive;
            description = "GID for mount point.";
            # TODO do better
            default = 100;
            example = 1000;
          };
          fsOptions = mkOption {
            type = types.listOf types.str;
            description = "Options for mounted filesystem.";
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
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ smbclient ];
    fileSystems = mapAttrs mkMount cfg.mounts;
  };
}
