{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.virt;
in {
  options.modules.services.virt = {
    enable = mkEnableOption false;
    client = mkOption {
      description = "Virtualization client options.";
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkEnableOption false;
        };
      });
    };
    server = mkOption {
      description = "Virtualization server options.";
      type = types.attrsOf (types.submodule {
        options = {
          enable = mkEnableOption false;
        };
      });
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      virtualisation.libvirtd.enable = true;
    }
    (mkIf (cfg.client.enable) {
      environment.systemPackages = with pkgs; [virt-manager kvm];
    })
    (mkIf (cfg.server.enable) {
      boot.kernelModules = ["kvm-intel" "kvm-amd"];

      virtualisation.libvirtd.allowedBridges = ["${cfg.libvert.bridgeInterface}"];

      networking.interfaces."${cfg.libvert.bridgeInterface}".useDHCP = true;

      networking.bridges = {
        "${cfg.libvert.bridgeInterface}" = {
          interfaces = ["${cfg.libvert.ethInterface}"];
        };
      };

      environment.systemPackages = with pkgs; [kvm];
    })
  ]);
}
