{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.reverseProxy;
  mkHost = localDomain: localAddress: domain: hostPort: {
    "${domain}" = {
      locations."/" = {
        proxyPass = "${localDomain}:${hostPort}";
      };
    };
  };
in {
  options.modules.services.reverseProxy = {
    enable = mkEnableOption false;
    localAddress = mkOption {
      description = "IP address of local server.";
      type = types.str;
    };
    localDomain = mkOption {
      description = "Local domain.";
      type = types.str;
      default = "home.lan";
    };
    locations = mkOption {
      description = "Mapping of ports to domains.";
      type = types.attrsOf (types.submodule {
        options = {
          hostPort = mkOption {
            description = "";
            type = types.int;
          };
          localDomain = mkOption {
            description = "";
            type = types.str;
          };
          subDomain = mkOption {
            description = "";
            type = types.str;
          };
        };
      });
    };
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = true;
      virtualHosts = mapAttrs (mkHost cfg.localAddress) cfg.locations;
    };
  };
}
