{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.calibre-mount;
in {
  options.modules.services.calibre-mount = {
    enable = mkEnableOption false;
    mountPoint = mkOption {
      description = "Mount point for calibre samba FS.";
      type = types.path;
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ smbclient ];

    fileSystems."${cfg.mountPoint}" = {
      device = "//192.168.86.31/calibre";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts =
          "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      in [
        "${automount_opts},credentials=/etc/nixos/smb-secrets,uid=1000,gid=100"
      ];
    };
  };
}
