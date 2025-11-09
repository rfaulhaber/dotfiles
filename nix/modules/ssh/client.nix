{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.ssh.client;
in {
  options.modules.services.ssh.client = {
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

  config = mkIf cfg.enable {
    home.programs.ssh = {
      enable = true;

      matchBlocks = let
        mkLocalHostname = n: "192.168.0.${n}";
        sshPath = cfg.sshPath;
        defaultIdentityFile = "${sshPath}/id_host";
      in {
        "*" = {
          identitiesOnly = true;
          identityFile = defaultIdentityFile;
          hashKnownHosts = true;
          addKeysToAgent = "yes";

          # home-manager's programs.ssh default configuration
          forwardAgent = false;
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };

        "atlas" = {
          hostname = mkLocalHostname "3";
          user = config.user.name;
          port = 10222;
          forwardAgent = true;
        };

        "atlas-netbird" = {
          hostname = "atlas.netbird.selfhosted";
          user = config.user.name;
          port = 10222;
          forwardAgent = true;
        };

        "codeberg.org" = {
          hostname = "codeberg.org";
          extraOptions = {
            "PreferredAuthentications" = "publickey";
            "AddressFamily" = "inet";
          };
        };

        "github.com" = {
          hostname = "github.com";
          extraOptions."PreferredAuthentications" = "publickey";
        };

        "gitlab.com" = {
          hostname = "github.com";
          extraOptions."PreferredAuthentications" = "publickey";
        };

        "git.3679.space" = {
          hostname = "git.3679.space";
          port = 3402;
          extraOptions."PreferredAuthentications" = "publickey";
        };

        "pallas" = {
          hostname = mkLocalHostname "2";
          forwardAgent = true;
          user = "ryan";
          port = 12981;
        };

        "pallas-netbird" = {
          hostname = "pallas.netbird.selfhosted";
          forwardAgent = true;
          user = "ryan";
          port = 12981;
        };

        "sourcehut" = {
          hostname = "*sr.ht";
          extraOptions = {
            "PreferredAuthentications" = "publickey";
            "AddressFamily" = "inet";
          };
        };

        "nix-installer" = {
          hostname = mkLocalHostname "190";
          user = "nixos";
          extraOptions."AddKeysToAgent" = "yes";
        };

        "steamdeck" = {
          hostname = "steamdeck";
          user = "deck";
          forwardAgent = true;
          port = 27077;
        };

        "nike" = {
          hostname = mkLocalHostname "227";
          user = "ryan";
          forwardAgent = true;
          port = 14625;
        };

        "janus" = {
          hostname = "66.63.168.153";
          user = config.user.name;
          forwardAgent = true;
          port = 6674;
        };

        "janus-netbird" = {
          hostname = "janus.netbird.selfhosted";
          user = config.user.name;
          forwardAgent = true;
          port = 6674;
        };
      };
    };
  };
}
