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
    };

    # TODO support multiple users?
    user.openssh.authorizedKeys.keys = mkIf cfg.enableServer cfg.keys;

    home.programs.ssh = mkIf cfg.enableClient {
      enable = true;
      compression = true;
      hashKnownHosts = true;

      matchBlocks = let sshPath = "${config.user.home}/.ssh";
      in {
        "*" = { identitiesOnly = true; };

        "atlas" = {
          hostname = "192.168.86.63";
          identityFile = "${sshPath}/id_nil2";
          user = config.user.name;
        };

        "nil" = {
          hostname = "192.168.86.31";
          identityFile = "${sshPath}/id_nil";
          user = config.user.name;
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
      };
    };

    security.pam.enableSSHAgentAuth = mkIf cfg.enableClient true;
  };
}
