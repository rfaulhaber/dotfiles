{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.nfs.serve;
  mkExportLine = _name: exportCfg: "${exportCfg.path} ${exportCfg.clients}";
in {
  options.modules.services.nfs.serve = {
    enable = mkEnableOption "NFS server";

    openFirewall = mkOption {
      description = "Whether to open firewall port 2049 for NFS.";
      type = types.bool;
      default = true;
    };

    interface = mkOption {
      description = "Network interface to open NFS firewall ports on.";
      type = types.str;
      example = "enp3s0";
    };

    exports = mkOption {
      description = "NFS exports to serve. Attribute names are identifiers (not used in config).";
      type = types.attrsOf (types.submodule {
        options = {
          path = mkOption {
            description = "Host path to export.";
            type = types.str;
            example = "/data/movies";
          };
          clients = mkOption {
            description = "NFS client spec string (hosts and options).";
            type = types.str;
            example = "192.168.0.0/24(rw,sync,no_subtree_check)";
          };
        };
      });
      default = {};
    };
  };

  config = mkIf cfg.enable {
    services.nfs.server = {
      enable = true;
      exports = concatStringsSep "\n" (mapAttrsToList mkExportLine cfg.exports);
    };

    networking.firewall.interfaces.${cfg.interface} = mkIf cfg.openFirewall {
      allowedTCPPorts = [2049];
      allowedUDPPorts = [2049];
    };
  };
}
