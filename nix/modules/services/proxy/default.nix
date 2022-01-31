{ config, lib, pkgs, ... }:

# this installs nginx and sets up a proxy server

with lib;

let
  cfg = config.modules.services.proxy;
  mkConfig = name: value: {
    locations."/" = { proxyPass = "http://127.0.0.1:${toString value}"; };
  };
in {
  options.modules.services.proxy = {
    enable = mkEnableOption false;
    aliases = mkOption {
      description =
        "Describes mapping between hostnames and port destinations on the server.";
      type = types.attrs;
      example = { "foo.example.com" = 8089; };
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
    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      # recommendedTlsSettings = true;
      appendHttpConfig = ''
        ${concatMapStringsSep "\n" (val: "allow ${val};") cfg.whitelist}
          deny all;
      '';
      virtualHosts = mapAttrs mkConfig cfg.aliases;
    };
  };
}
