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
        };

        "atlas" = {
          hostname = mkLocalHostname "3";
          user = config.user.name;
          port = 10222;
          forwardAgent = true;
          extraOptions."AddKeysToAgent" = "yes";
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

        "git.3679.space" = {
          hostname = "git.3679.space";
          port = 3402;
          extraOptions = {
            "PreferredAuthentications" = "publickey";
            "AddKeysToAgent" = "yes";
          };
        };

        "pallas" = {
          hostname = mkLocalHostname "2";
          forwardAgent = true;
          user = "ryan";
          port = 12981;
          extraOptions."AddKeysToAgent" = "yes";
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
          extraOptions."AddKeysToAgent" = "yes";
        };

        "nike" = {
          hostname = mkLocalHostname "227";
          user = "ryan";
          forwardAgent = true;
          port = 14625;
          extraOptions."AddKeysToAgent" = "yes";
        };

        "janus" = {
          hostname = "66.63.168.153";
          user = "ryan";
          forwardAgent = true;
          port = 6674;
          extraOptions."AddKeysToAgent" = "yes";
        };
      };
    };
  };
}
