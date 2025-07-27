{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.kubernetes;
in {
  options.modules.services.kubernetes = {
    client = {
      enable = mkOption {
        description = "If true, allows control of Kubernetes on this machine.";
        type = types.bool;
        default = false;
      };
    };

    node = {
      enable = mkOption {
        description = "If true, this machine is a node in the Kubernetes cluster.";
        type = types.bool;
        default = false;
      };

      isMaster = mkOption {
        description = "If true, this node is the master node.";
        type = types.bool;
        default = false;
      };
    };

    master = {
      ip = mkOption {
        description = "IP address of master node.";
        type = types.str;
        default = null;
      };

      port = {
        description = "Port number of master API";
        type = types.int;
        default = null;
      };

      hostname = {
        description = "Hostname of master node.";
        type = types.str;
        default = null;
      };
    };

    package = mkOption {
      description = "Kubernetes package to use.";
      type = types.package;
      default = pkgs.kubernetes;
    };
  };

  config = mkMerge [
    {
      assertions = [
        {
          assertion = !(cfg.client.enable && cfg.node.enable);
          message = "Machine cannot be both a node and a client.";
        }
      ];
    }
    (mkIf (cfg.client.enable) {
      user.packages = with pkgs; [
        kompose
        kubectl
        kubernetes
      ];
    })
    (mkIf (cfg.node.enable) {
      services.kubernetes = let
        masterAddress = "http://${cfg.master.host}:${toString cfg.master.port}";
      in {
        roles = ["node"];

        masterAddress = cfg.masterHost;
        easyCerts = true;

        kubelet.kubeconfig.server = api;
        apiserverAddress = api;

        addons.dns.enable = true;

        package = cfg.package;
      };
    })
    (mkIf (cfg.node.enable && cfg.node.isMaster) {
      services.kubernetes = {
        roles = ["node" "master"];
        apiserver = {
          securePort = cfg.master.port;
          advertiseAddress = kubeMasterIP;
        };
      };
    })
  ];
}
