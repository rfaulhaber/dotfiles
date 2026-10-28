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
      lib.optionals cfg.useDefaultKeys (import ./keys.nix) ++ cfg.extraKeys;
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
        # Only the declared keys in /etc/ssh/authorized_keys.d grant login, so
        # anything running as the user can't plant one that outlives a rebuild.
        authorizedKeysInHomedir = false;
        settings = {
          PasswordAuthentication = false;
          # Otherwise any auth module later added to sshd's PAM stack would
          # reopen password-style login despite PasswordAuthentication.
          KbdInteractiveAuthentication = false;
          PermitRootLogin = lib.mkDefault "no";
          AllowTcpForwarding = lib.mkDefault true;
        };
        inherit (cfg) extraConfig;
        ports = [cfg.port];
      };

      # Make Ghostty terminfo available for incoming SSH sessions. When
      # ghostty itself is in the closure (desktop hosts) the multi-output
      # `.terminfo` reference is free. On headless hosts, referencing it
      # would force a full ghostty rebuild (gtk4, libadwaita, pipewire,
      # gstreamer, zig stack) any time cache.nixos.org doesn't have the
      # current commit — 90+ minutes on aarch64. Compile the vendored
      # terminfo source instead; pulls only ncurses.
      environment.systemPackages = [
        (
          if config.modules.programs.ghostty.enable
          then pkgs.ghostty.terminfo
          else
            pkgs.runCommandLocal "ghostty-terminfo" {
              nativeBuildInputs = [pkgs.ncurses];
            } ''
              mkdir -p $out/share/terminfo
              tic -x -o $out/share/terminfo ${./ghostty.terminfo}
            ''
        )
      ];

      # TODO support multiple users?
      user.openssh.authorizedKeys.keys = keys;

      # nixpkgs' sudo and sudo-rs modules opt their own PAM service in; this
      # only switches pam_ssh_agent_auth on.
      security.pam.sshAgentAuth.enable = true;
    };
}
