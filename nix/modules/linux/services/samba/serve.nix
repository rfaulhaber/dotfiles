{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.samba.serve;
  anyGuestShares = any (s: s.guestOk) (attrValues cfg.shares);
  mkShare = name: shareCfg: let
    baseSettings = {
      inherit (shareCfg) path;
      comment =
        if shareCfg.comment == null
        then name
        else shareCfg.comment;
      "read only" =
        if shareCfg.readOnly
        then "yes"
        else "no";
      browsable = "yes";
      "guest ok" =
        if shareCfg.guestOk
        then "yes"
        else "no";
    };
    userSettings = optionalAttrs (!shareCfg.guestOk) {
      "valid users" = concatStringsSep ", " cfg.validUsers;
    };
  in
    baseSettings // userSettings // shareCfg.extraOptions;
in {
  options.modules.services.samba.serve = {
    enable = mkEnableOption "Samba file sharing";

    package = mkOption {
      description = "The Samba package to use.";
      type = types.package;
      default = pkgs.samba;
      example = "pkgs.sambaFull";
    };

    subnet = mkOption {
      description = "Subnet allowed to access Samba shares. Uses Samba 'hosts allow' syntax.";
      type = types.str;
      example = "192.168.1.";
    };

    validUsers = mkOption {
      description = "List of users allowed to access non-guest shares by default.";
      type = types.listOf types.str;
      default = [config.user.name];
      example = ["ryan" "guest"];
    };

    openFirewall = mkOption {
      description = "Whether to open firewall ports for Samba (TCP 445/139, UDP 137/138).";
      type = types.bool;
      default = true;
    };

    interface = mkOption {
      description = "Network interface to open Samba firewall ports on.";
      type = types.str;
      example = "enp3s0";
    };

    extraGlobalSettings = mkOption {
      description = "Extra settings to add to the Samba [global] section.";
      type = types.attrsOf types.str;
      default = {};
      example = {"server string" = "NAS";};
    };

    shares = mkOption {
      description = "Shares to host via Samba.";
      type = types.attrsOf (types.submodule {
        options = {
          path = mkOption {
            description = "Path to the directory to share.";
            type = types.str;
            example = "/data/calibre";
          };
          comment = mkOption {
            description = "Description of the share. Defaults to the share name if unset.";
            type = types.nullOr types.str;
            default = null;
            example = "Calibre library share.";
          };
          readOnly = mkOption {
            description = "Whether the share is read-only.";
            type = types.bool;
            default = false;
          };
          guestOk = mkOption {
            description = "Whether to allow unauthenticated guest access to this share.";
            type = types.bool;
            default = false;
          };
          extraOptions = mkOption {
            description = "Extra Samba options for this share. Can override defaults.";
            type = types.attrsOf types.str;
            default = {};
            example = {"force user" = "nobody";};
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    services.samba = {
      enable = true;
      package = cfg.package;
      settings =
        {
          global =
            {
              "hosts allow" = cfg.subnet;
              "hosts deny" = "0.0.0.0/0 ::/0";
              "server min protocol" = "SMB3";
              security = "user";
            }
            // optionalAttrs anyGuestShares {
              "map to guest" = "Bad User";
            }
            // cfg.extraGlobalSettings;
        }
        // (mapAttrs mkShare cfg.shares);
    };

    networking.firewall.interfaces.${cfg.interface} = mkIf cfg.openFirewall {
      allowedTCPPorts = [445 139];
      allowedUDPPorts = [137 138];
    };
  };
}
