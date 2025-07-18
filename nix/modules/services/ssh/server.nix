{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.ssh.server;
in {
  options.modules.services.ssh.server = {
    enable = mkOption {
      description = "If true, enables SSH server.";
      type = types.bool;
      default = false;
    };

    keys = mkOption {
      description = "SSH keys used by the default user of this machine.";
      default = import ./keys.nix;
      type = types.listOf types.str;
    };

    port = mkOption {
      description = "SSH port to use.";
      default = 10222;
      type = types.int;
    };

    extraConfig = mkOption {
      description = "Extra config for this host.";
      type = types.str;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (builtins.length cfg.keys) > 0;
        message = "SSH client must have at least one authorized key";
      }
    ];
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = lib.mkDefault "no";
      };
      extraConfig =
        ''
          PermitEmptyPasswords no
          AllowTcpForwarding yes
        ''
        + cfg.extraConfig;
      ports = [cfg.port];
    };

    # TODO support multiple users?
    user.openssh.authorizedKeys.keys = cfg.keys;

    security.pam = {
      # TODO are both necessary?
      sshAgentAuth.enable = true;
      services.${config.user.name}.sshAgentAuth = true;
    };
  };
}
