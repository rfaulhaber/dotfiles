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
  # TODO create hostnames on ZeroTier network for mobile devices
  options.modules.services.ssh = {
    enable = mkEnableOption false;

    client = {
      enable = mkOption {
        description = "If true, enables SSH client.";
        type = types.bool;
        default = false;
      };

      sshPath = mkOption {
        description = "Path to SSH directory.";
        type = types.either types.str types.path;
        default = "${config.user.home}/.ssh";
        apply = toString;
      };
    };

    server = {
      enable = mkOption {
        description = "If true, enables SSH server.";
        type = types.bool;
        default = false;
      };

      keys = mkOption {
        description = "SSH keys used by the default user of this machine.";
        default = [];
        type = types.listOf types.str;
      };

      port = mkOption {
        description = "SSH port to use.";
        default = 10222;
        type = types.int;
      };
    };
  };

  config = mkIf cfg.enable {
    services.openssh = mkIf cfg.server.enable {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
      extraConfig = ''
        PermitEmptyPasswords no
        AllowTcpForwarding yes
      '';
      ports = [cfg.server.port];
    };

    # TODO support multiple users?
    # TODO make more secure, see https://github.com/NixOS/nixpkgs/issues/31611
    user.openssh.authorizedKeys.keys = mkIf cfg.server.enable cfg.server.keys;

    security.pam.sshAgentAuth.enable = mkIf cfg.server.enable true;

    # TODO define hosts externally?
    home.programs.ssh = mkIf cfg.client.enable {
      enable = true;
      compression = true;
      hashKnownHosts = true;

      matchBlocks = let
        mkLocalHostname = n: "192.168.0.${n}";
        sshPath = cfg.client.sshPath;
      in {
        "*".identitiesOnly = true;

        "atlas" = {
          hostname = mkLocalHostname "3";
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

        "pallas" = {
          hostname = mkLocalHostname "2";
          identityFile = "${sshPath}/id_pallas";
          user = "ryan";
          extraOptions = {"AddKeysToAgent" = "yes";};
          port = 11689;
        };
      };
    };
  };
}
