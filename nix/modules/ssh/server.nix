{
  config,
  lib,
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

    useDefaultKeys = mkOption {
      description = "If true, loads the keys in keys.nix.";
      type = types.bool;
      default = true;
    };

    extraKeys = mkOption {
      description = "Additional SSH keys to configure.";
      type = types.listOf types.str;
      default = [];
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

  config = let
    keys =
      if cfg.useDefaultKeys
      then (import ./keys.nix) ++ cfg.extraKeys
      else cfg.extraKeys;
  in
    mkIf cfg.enable {
      assertions = [
        {
          assertion = (builtins.length keys) > 0;
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
      user.openssh.authorizedKeys.keys = keys;

      security.pam = {
        # TODO are both necessary?
        sshAgentAuth.enable = true;
        services.${config.user.name}.sshAgentAuth = true;
      };
    };
}
