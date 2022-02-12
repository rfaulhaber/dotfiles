{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = {
    enable = mkEnableOption false;
    keys = mkOption {
      description = "SSH keys used by the default user of this machine.";
      default = [ ];
      type = types.listOf types.str;
    };
    enableClient = mkEnableOption false;
    enableServer = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services.openssh = mkIf cfg.enableServer {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
      extraConfig = ''
        PermitEmptyPasswords no
      '';
      ports = [ 10222 ];
    };

    # TODO support multiple users?
    # TODO make configurable from outside?
    user.openssh.authorizedKeys.keys = mkIf cfg.enableServer cfg.keys;

    security.pam.enableSSHAgentAuth = mkIf cfg.enableServer true;

    home.programs.ssh = mkIf cfg.enableClient {
      enable = true;
      compression = true;
      hashKnownHosts = true;

      matchBlocks = let
        sshPath = "${config.user.home}/.ssh";
        mkLocalHostname = n: "192.168.86.${n}";
      in {
        "*" = { identitiesOnly = true; };

        "atlas" = {
          hostname = mkLocalHostname "63";
          identityFile = "${sshPath}/id_nil2";
          user = config.user.name;
          port = 10222;
          extraOptions = { "AddKeysToAgent" = "yes"; };
        };

        "github.com" = {
          hostname = "github.com";
          identityFile = "${sshPath}/id_github";
          extraOptions = { "PreferredAuthentications" = "publickey"; };
        };

        "gitlab.com" = {
          hostname = "github.com";
          identityFile = "${sshPath}/id_gitlab";
          extraOptions = { "PreferredAuthentications" = "publickey"; };
        };

        "pi" = {
          hostname = mkLocalHostname "79";
          identityFile = "${sshPath}/id_pi";
          user = "pi";
          extraOptions = { "AddKeysToAgent" = "yes"; };
        };
      };
    };

  };
}
