{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.calibre-serve;
in {
  options.modules.services.calibre-serve = {
    enable = mkEnableOption false;
    sharePoint = mkOption {
      description = "Path to serve for calibre samba FS.";
      type = types.path;
    };
  };

  config = mkIf cfg.enable {
    services.samba = {
      enable = true;
      securityType = "user";
      shares = {
        calibre = {
          path = toString cfg.sharePoint;
          comment = "Calibre folder.";
          "read only" = "no";
          browsable = "yes";
          "guest ok" = "no";
          "valid users" = config.user.name;
        };
      };
      # samba should only be accessible on the local network
      extraConfig = ''
        hosts allow = 192.168.86.
        hosts deny = 0.0.0.0/0

        server min protocol = SMB3
      '';
    };

    networking.firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ 445 139 ];
      allowedUDPPorts = [ 137 138 ];
    };
  };
}
