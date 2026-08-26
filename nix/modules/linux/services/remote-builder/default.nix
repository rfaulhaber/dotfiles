{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge types;
  cfg = config.modules.services.remote-builder;

  # Both halves of the pair agree on these. vulcan is pinned by IP for the
  # same reason every LAN host alias in ssh/client.nix is: .lan AAAA records
  # have black-holed LAN traffic before.
  builderUser = "nixremote";
  builderHost = "192.168.0.105"; # vulcan
  builderPort = 13308;
  builderHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIxqfiVtzbwTehEsXuYFXBAIUkMhQtKG4q4apRm4Fite";
in {
  options.modules.services.remote-builder = {
    server = {
      enable = mkEnableOption "serving remote Nix builds over SSH";
      authorizedKeys = mkOption {
        description = "SSH public keys allowed to submit builds as the builder user.";
        type = types.listOf types.str;
        default = [];
      };
    };
    client = {
      enable = mkEnableOption "dispatching Nix builds to vulcan";
      sshKey = mkOption {
        description = ''
          Path to the private key for the builder connection, readable by
          root (the build hook runs as the daemon) — i.e. a sops secret path.
        '';
        type = types.str;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.server.enable {
      assertions = [
        {
          assertion = config.modules.services.ssh.server.enable;
          message = "remote-builder.server requires modules.services.ssh.server";
        }
      ];

      users = {
        users.${builderUser} = {
          isSystemUser = true;
          group = builderUser;
          # sshd refuses exec for shell-less accounts; builds arrive as a
          # single non-interactive `nix-daemon --stdio` exec.
          shell = pkgs.bashInteractive;
          # `restrict` drops pty, forwarding, and X11 — command exec over
          # stdio is all the build protocol needs.
          openssh.authorizedKeys.keys = map (key: "restrict ${key}") cfg.server.authorizedKeys;
        };
        groups.${builderUser} = {};
      };

      # Trusted so unsigned store paths uploaded from the client are
      # accepted. That is root-equivalent on this daemon, which is fine for
      # the same reason the CI runners must never be: runners execute
      # third-party job code, while this key is held only by root on
      # hyperion, which already holds deploy keys for the whole fleet.
      nix.settings.trusted-users = [builderUser];
    })

    (mkIf cfg.client.enable {
      nix = {
        # The build hook prefers any free remote slot over an idle local
        # machine; local building is the overflow once vulcan's slots fill.
        # Wide fan-outs therefore still lean on the local max-jobs pool,
        # but a lone derivation ships to vulcan even when this host is
        # idle — `--builders ''` pins an invocation local. Listing this
        # machine itself as a builder to invert the preference deadlocks:
        # output locks are held across the hook run (upstream FIXME in
        # derivation-building-goal.cc).
        distributedBuilds = true;
        buildMachines = [
          {
            hostName = "vulcan-builder";
            sshUser = builderUser;
            sshKey = cfg.client.sshKey;
            protocol = "ssh-ng";
            systems = ["x86_64-linux"];
            # Bounded so dispatched builds coexist with the four CI runners;
            # each job still gets every core via NIX_BUILD_CORES.
            maxJobs = 4;
            supportedFeatures = ["big-parallel" "kvm" "nixos-test" "benchmark"];
          }
        ];
        # Let the builder pull dependencies from its own substituters
        # instead of the client uploading them over the same wire first.
        settings.builders-use-substitutes = true;
      };

      # The build hook runs as root, which has no home-manager ssh config;
      # the alias and pinned host key live at the system level so the first
      # dispatch never stalls on an interactive TOFU prompt.
      programs.ssh = {
        extraConfig = ''
          Host vulcan-builder
            HostName ${builderHost}
            Port ${toString builderPort}
            IdentitiesOnly yes
        '';
        knownHosts.vulcan-builder = {
          hostNames = ["[${builderHost}]:${toString builderPort}"];
          publicKey = builderHostKey;
        };
      };
    })
  ];
}
