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
  # refer to https://jellyfin.org/docs/general/networking/nginx/
  # example usage:
  # {
  #   reverseProxy = {
  #     enable = true;
  #     localDomain = "home.lan";
  #     localAddress = "192.168.0.2";
  #     locations = {
  #       "jellyfin" = { # should resolve to both jellyfin.home.lan and jellyfin.3679.space
  #         target = "192.168.0.3:8086";
  #         remoteDomains = [ "3679.space" ];
  #         whitelist = [
  #           # external ip addresses to whitelist
  #           # TODO load whitelist IPs from secrets
  #         ];
  #       };
  #     };
  #   }
  # }
  options.modules.services.reverseProxy = {
    enable = mkEnableOption false;
    localAddress = mkOption {
      description = "IP address of local server.";
      type = types.str;
      required = true;
    };
    localDomain = mkOption {
      description = "Local domain.";
      type = types.str;
      default = "home.lan";
    };
    locations = mkOption {
      description = "Mapping of addresses to domains.";
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
