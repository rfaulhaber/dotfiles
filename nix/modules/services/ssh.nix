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
  # TODO split into separate modules, ssh.server and ssh.client
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

      extraConfig = mkOption {
        description = "Extra config for this host.";
        type = types.str;
        default = "";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # ssh server config
    (mkIf (cfg.server.enable) {
      assertions = [
        {
          assertion = (builtins.length cfg.server.keys) > 0;
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
          + cfg.server.extraConfig;
        ports = [cfg.server.port];
      };

      # TODO support multiple users?
      # TODO make more secure, see https://github.com/NixOS/nixpkgs/issues/31611
      user.openssh.authorizedKeys.keys = cfg.server.keys;

      security.pam = {
        # TODO are both necessary?
        sshAgentAuth.enable = true;
        services.${config.user.name}.sshAgentAuth = true;
      };
    })
    # ssh client config
    (mkIf (cfg.client.enable) {
      # TODO
      # programs.ssh.startAgent = true;

      home.programs.ssh = {
        enable = true;
        compression = true;
        hashKnownHosts = true;
        addKeysToAgent = "yes";

        matchBlocks = let
          mkLocalHostname = n: "192.168.0.${n}";
          sshPath = cfg.client.sshPath;
          defaultIdentityFile = "${sshPath}/id_host";
        in {
          "*" = {
            identitiesOnly = true;
            identityFile = defaultIdentityFile;
          };

          "atlas" = {
            hostname = mkLocalHostname "3";
            user = config.user.name;
            port = 10222;
            forwardAgent = true;
            extraOptions = {
              "AddKeysToAgent" = "yes";
            };
          };

          "github.com" = {
            hostname = "github.com";
            extraOptions = {
              "PreferredAuthentications" = "publickey";
              "AddKeysToAgent" = "yes";
            };
          };

          "gitlab.com" = {
            hostname = "github.com";
            extraOptions = {
              "PreferredAuthentications" = "publickey";
              "AddKeysToAgent" = "yes";
            };
          };

          "pallas" = {
            hostname = mkLocalHostname "2";
            forwardAgent = true;
            user = "ryan";
            extraOptions = {
              "AddKeysToAgent" = "yes";
            };
            port = 12981;
          };

          "nike" = {
            hostname = mkLocalHostname "77";
            forwardAgent = true;
            user = "ryan";
            extraOptions = {
              "AddKeysToAgent" = "yes";
            };
            port = 14625;
          };

          "nix-installer" = {
            hostname = mkLocalHostname "190";
            user = "nixos";
            extraOptions = {
              "AddKeysToAgent" = "yes";
            };
          };
        };
      };
    })
  ]);
}
