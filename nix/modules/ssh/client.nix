{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}: let
  inherit (lib) mkOption types mkIf optional optionalAttrs mapAttrs;
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

    deployAgent.enable = mkOption {
      description = ''
        If true, run a dedicated ssh-agent holding only the janus deploy
        key and forward it to janus, restoring pam_ssh_agent_auth sudo
        there without exposing the fleet agent. The agent service is
        Linux-only (systemd user unit).
      '';
      type = types.bool;
      default = false;
    };
  };

  config = let
    deployAgentSocket = "${cfg.sshPath}/janus-deploy.sock";
    deployAgentKey = "${cfg.sshPath}/id_janus_deploy";
    deployForward = optionalAttrs cfg.deployAgent.enable {
      ForwardAgent = deployAgentSocket;
    };
    hostKeys = import ./host-keys.nix;
    netbirdName = name: "${name}.netbird.selfhosted";
    # Where each host's sshd listens. janus has no LAN address because it
    # accepts SSH only over netbird.
    fleet = {
      atlas = {
        address = "192.168.0.3";
        port = 10222;
      };
      hecate = {
        address = "192.168.0.77";
        port = 17263;
      };
      janus = {
        address = null;
        port = 6674;
      };
      pallas = {
        address = "192.168.0.2";
        port = 12981;
      };
      prometheus = {
        address = "192.168.0.228";
        port = 13571;
      };
      vulcan = {
        address = "192.168.0.105";
        port = 13308;
      };
    };
    # An unknown or changed host key fails outright instead of prompting: a
    # prompt accepted for an impostor would hand it the forwarded agent.
    fleetAlias = name: hostName: {
      HostName = hostName;
      User = config.user.name;
      Port = fleet.${name}.port;
      StrictHostKeyChecking = "yes";
    };
    lanAlias = name: fleetAlias name fleet.${name}.address // {ForwardAgent = true;};
    netbirdAlias = name: fleetAlias name (netbirdName name) // {ForwardAgent = true;};
  in
    mkIf cfg.enable ({
        programs.ssh.knownHosts =
          mapAttrs (name: host: {
            hostNames =
              map (h: "[${h}]:${toString host.port}")
              (optional (host.address != null) host.address ++ [(netbirdName name)]);
            publicKey = hostKeys.${name};
          })
          fleet;

        home.programs.ssh = {
          enable = true;
          enableDefaultConfig = false;

          settings = let
            mkLocalHostname = n: "192.168.0.${n}";
            inherit (cfg) sshPath;
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

            "atlas" = lanAlias "atlas";
            "atlas-netbird" = netbirdAlias "atlas";

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

            "pallas" = lanAlias "pallas";
            "pallas-netbird" = netbirdAlias "pallas";

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

            # The fleet agent is never forwarded to janus: it terminates
            # untrusted internet traffic, so root there could use a forwarded
            # agent socket as a signing oracle against every host that trusts
            # these keys. With deployAgent enabled we instead forward a
            # dedicated agent holding only the janus deploy key, giving
            # pam_ssh_agent_auth a key to challenge for sudo while the socket
            # grants nothing janus's root doesn't already have.
            #
            # janus accepts no SSH on its public address; if the overlay is
            # down, the provider's web console is the way in.
            "janus" = fleetAlias "janus" (netbirdName "janus") // deployForward;

            "hecate" = lanAlias "hecate";
            "vulcan" = lanAlias "vulcan";
            "prometheus" = lanAlias "prometheus";
          };
        };
      }
      // optionalAttrs (!isDarwin) {
        systemd.user.services.janus-deploy-agent = mkIf cfg.deployAgent.enable {
          description = "Scoped ssh-agent for janus deploy sudo";
          wantedBy = ["default.target"];
          unitConfig = {
            ConditionUser = "!@system";
            ConditionPathExists = deployAgentKey;
          };
          serviceConfig = {
            Type = "forking";
            Environment = "SSH_AUTH_SOCK=${deployAgentSocket}";
            ExecStartPre = "${pkgs.coreutils}/bin/rm -f ${deployAgentSocket}";
            ExecStart = "${pkgs.openssh}/bin/ssh-agent -a ${deployAgentSocket}";
            # The deploy key is deliberately passphrase-less so this add is
            # non-interactive; its only authority is login/sudo on janus,
            # and hyperion compromise already yields the primary agent.
            ExecStartPost = "${pkgs.openssh}/bin/ssh-add ${deployAgentKey}";
            StandardOutput = "null";
            Restart = "on-failure";
            SuccessExitStatus = "0 2";
          };
        };
      });
}
