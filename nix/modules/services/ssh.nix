{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.ssh;
  _1passwordEnable = config.modules.programs._1password.enable;
in {
  options.modules.services.ssh = {
    enable = mkEnableOption false;
    keys = mkOption {
      description = "SSH keys used by the default user of this machine.";
      default = [];
      type = types.listOf types.str;
    };
    # TODO split into two separate modules
    enableClient = mkEnableOption false;
    # TODO create hostnames on ZeroTier network for mobile devices
    enableServer = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    services.openssh = mkIf cfg.enableServer {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
      extraConfig = ''
        PermitEmptyPasswords no
        AllowTcpForwarding yes
      '';
      ports = [10222];
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
        mkLocalHostname = n: "192.168.0.${n}";
      in {
        "*".identitiesOnly = true;

        "atlas" = {
          hostname = mkLocalHostname "2";
          identityFile = "${sshPath}/id_atlas";
          user = config.user.name;
          port = 10222;
          extraOptions = {"AddKeysToAgent" = "yes";};
        };

        "github.com" = {
          hostname = "github.com";
          identityFile = "${sshPath}/id_github";
          extraOptions = {
            "PreferredAuthentications" = "publickey";
            "AddKeysToAgent" = "yes";
          };
        };

        "gitlab.com" = {
          hostname = "github.com";
          identityFile = "${sshPath}/id_gitlab";
          extraOptions = {
            "PreferredAuthentications" = "publickey";
            "AddKeysToAgent" = "yes";
          };
        };

        "pi" = {
          hostname = mkLocalHostname "70";
          identityFile = "${sshPath}/id_pi";
          user = "pi";
          extraOptions = {"AddKeysToAgent" = "yes";};
          port = 2222;
        };
      };
    };
  };
}
