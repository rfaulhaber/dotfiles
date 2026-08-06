{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}: let
  inherit (lib) mkOption types mkIf optionalAttrs;
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
      apply = builtins.toString;
    };
  };

  config = mkIf cfg.enable {
    home.programs.ssh = {
      enable = true;
      enableDefaultConfig = false;

      settings = let
        mkLocalHostname = n: "192.168.0.${n}";
        sshPath = cfg.sshPath;
        defaultIdentityFile = "${sshPath}/id_host";
      in {
        "*" =
          {
            IdentitiesOnly = true;
            IdentityFile = defaultIdentityFile;
            HashKnownHosts = true;
            AddKeysToAgent = "yes";

            # home-manager's programs.ssh default configuration
            ForwardAgent = false;
            Compression = false;
            ServerAliveInterval = 0;
            ServerAliveCountMax = 3;
            UserKnownHostsFile = "~/.ssh/known_hosts";
            ControlMaster = "no";
            ControlPath = "~/.ssh/master-%r@%n:%p";
            ControlPersist = "no";
          }
          // optionalAttrs isDarwin {
            # macOS: use Keychain for SSH key passphrases
            UseKeychain = "yes";
          };

        "atlas" = {
          HostName = mkLocalHostname "3";
          User = config.user.name;
          Port = 10222;
          ForwardAgent = true;
        };

        "atlas-netbird" = {
          HostName = "atlas.netbird.selfhosted";
          User = config.user.name;
          Port = 10222;
          ForwardAgent = true;
        };

        "codeberg.org" = {
          HostName = "codeberg.org";
          PreferredAuthentications = "publickey";
          AddressFamily = "inet";
        };

        "github.com" = {
          HostName = "github.com";
          PreferredAuthentications = "publickey";
        };

        "gitlab.com" = {
          HostName = "gitlab.com";
          PreferredAuthentications = "publickey";
        };

        "git.3679.space" = {
          HostName = "git.3679.space";
          Port = 3402;
          PreferredAuthentications = "publickey";
        };

        "pallas" = {
          HostName = mkLocalHostname "2";
          ForwardAgent = true;
          User = "ryan";
          Port = 12981;
        };

        "pallas-netbird" = {
          HostName = "pallas.netbird.selfhosted";
          ForwardAgent = true;
          User = "ryan";
          Port = 12981;
        };

        "*sr.ht" = {
          PreferredAuthentications = "publickey";
          AddressFamily = "inet";
        };

        "nix-installer" = {
          HostName = mkLocalHostname "190";
          User = "nixos";
          AddKeysToAgent = "yes";
        };

        "steamdeck" = {
          HostName = mkLocalHostname "226";
          User = "deck";
          ForwardAgent = true;
          Port = 27077;
        };

        # No ForwardAgent for janus: it terminates untrusted internet
        # traffic, so root there could use a forwarded agent socket as a
        # signing oracle against every host that trusts these keys.
        "janus" = {
          HostName = "66.63.168.153";
          User = config.user.name;
          Port = 6674;
        };

        "janus-netbird" = {
          HostName = "janus.netbird.selfhosted";
          User = config.user.name;
          Port = 6674;
        };

        "hecate" = {
          HostName = mkLocalHostname "77";
          ForwardAgent = true;
          User = "ryan";
          Port = 17263;
        };

        "vulcan" = {
          HostName = mkLocalHostname "105";
          ForwardAgent = true;
          User = "ryan";
          Port = 13308;
        };

        "prometheus" = {
          HostName = mkLocalHostname "228";
          ForwardAgent = true;
          User = "ryan";
          Port = 13571;
        };
      };
    };
  };
}
