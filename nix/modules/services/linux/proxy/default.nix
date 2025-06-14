{
  config,
  lib,
  pkgs,
  ...
}:
# this installs nginx and sets up a proxy server
# TODO this should probably be cleaned up
with lib; let
  cfg = config.modules.services.proxy;
  # for aliases in the home network
  mkConfig = name: value: {
    locations."/".proxyPass = "http://127.0.0.1:${toString value}";
  };
  # for publicly available aliases
  # TODO make more modular to support more domains, won't work with more than one
  mkExternalConfig = name: value:
    (mkConfig name value)
    // {
      forceSSL = true;
      enableACME = true;
    };
in {
  options.modules.services.proxy = {
    enable = mkEnableOption false;
    localAliases = mkOption {
      description = "Describes mapping between local hostnames and port destinations on the server. This will make each service a subdomain of the local domain name.";
      type = types.attrs;
      example = {"foo.home.lan" = 8089;};
    };
    externalAliases = mkOption {
      description = "Describes mapping between publicly available hostnames and port destinations on the server.";
      type = types.attrs;
      example = {"foo.example.com" = 8089;};
      default = {};
    };
    defaultAcmeEmail = mkOption {
      description = "Default email used for ACME";
      type = types.str;
      example = "foo@example.com";
    };
    # whitelist = mkOption {
    #   description =
    #     "Whitelisted IPv4 addressees for the proxy server to allow. By default denies everything else.";
    #   type = types.listOf types.str;
    #   example = [ "111.111.111.111" "222.222.222.222" ];
    #   default = [ ];
    # };
  };

  config = mkIf cfg.enable {
    assertions = let
      isNotBlank = s: s != null && s != "";
    in [
      {
        assertion =
          (length (attrNames cfg.localAliases))
          >= 1
          || (length (attrNames cfg.externalAliases)) >= 1;
        message = "You should specify at least one alias.";
      }
      {
        assertion =
          ((length (attrNames cfg.externalAliases) >= 1)
            && isNotBlank cfg.defaultEmail)
          || true;
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
      virtualHosts = let
        externalAliases = mapAttrs mkExternalConfig cfg.externalAliases;
        localAliases = mapAttrs mkConfig cfg.localAliases;
      in
        localAliases // externalAliases;
    };
    security.acme = mkIf ((length cfg.externalAliases) >= 1) {
      acceptTerms = true;
      defaults.email = cfg.defaultAcmeEmail;
    };

    # Linux-specific firewall configuration
    networking.firewall = mkIf pkgs.stdenv.isLinux {
      enable = true;
      allowedTCPPorts = let
        externalPorts = attrValues cfg.externalAliases;
        localPorts = attrValues cfg.localAliases;
      in
        [80 443] ++ localPorts ++ externalPorts;
    };
  };
}
