{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.services.samba-serve;
  mkShare = name:
    { path, comment ? name, ... }@shareOpts:
    {
      inherit path comment;
      "read only" = "no";
      browsable = "yes";
      "guest ok" = "no";
      "valid users" = config.user.name;
    } // shareOpts.extraOptions;
in {
  options.modules.services.samba-serve = {
    enable = mkEnableOption false;
    shares = mkOption {
      description = "Shares to host on Samba.";
      type = types.attrsOf (types.submodule {
        options = {
          path = mkOption {
            description = "Share point for Samba share.";
            type = types.either types.str types.path;
            example = /data/calibre;
          };
          comment = mkOption {
            description = "Samba comment.";
            type = types.str;
            example = "Calibre share pont.";
          };
          extraOptions = mkOption {
            description =
              "Extra Samba options. Can override defaults set in this module.";
            type = types.attrs;
            default = { };
            example = { "read only" = "yes"; };
          };
        };
      });
    };
  };

  config = mkIf cfg.enable {
    services.samba = {
      enable = true;
      securityType = "user";
      shares = mapAttrs mkShare cfg.shares;

      # samba should only be accessible on the local network
      extraConfig = ''
        hosts allow = 192.168.0.
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
