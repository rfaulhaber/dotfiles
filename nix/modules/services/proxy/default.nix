{ config, lib, pkgs, ... }:

# this installs nginx and sets up a proxy server

with lib;

let
  cfg = config.modules.services.proxy;
  mkConfig = name: value: {
    # TODO make more modular to support more domains, won't work with more than one
    forceSSL = true;
    enableACME = true;
    locations."/" = { proxyPass = "http://127.0.0.1:${toString value}"; };
  };
  isNotBlank = s: s != null && s != "";
in {
  options.modules.services.proxy = {
    enable = mkEnableOption false;
    aliases = mkOption {
      description =
        "Describes mapping between hostnames and port destinations on the server.";
      type = types.attrs;
      example = { "foo.example.com" = 8089; };
    };
    defaultAcmeEmail = mkOption {
      description = "Default email used for ACME";
      type = types.str;
      example = "foo@example.com";
    };
    whitelist = mkOption {
      description =
        "Whitelisted IPv4 addressees for the proxy server to allow. By default denies everything else.";
      type = types.listOf types.str;
      example = [ "111.111.111.111" "222.222.222.222" ];
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (length (attrNames cfg.aliases)) >= 1;
        message = "You should specify at least one alias.";
      }
      {
        assertion = isNotBlank cfg.defaultAcmeEmail;
        message = "You must specify an email to be used for ACME.";
      }
    ];
    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      # appendHttpConfig = ''
      #   ${concatMapStringsSep "\n" (val: "allow ${val};") cfg.whitelist}
      #     deny all;
      # '';
      virtualHosts = mapAttrs mkConfig cfg.aliases;
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = cfg.defaultAcmeEmail;
    };

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 80 443 ] ++ attrValues cfg.aliases;
    };
  };
}
