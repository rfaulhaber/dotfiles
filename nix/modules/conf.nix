{
  config,
  lib,
  isLinux,
  ...
}: let
  cfg = config.modules.nix;
in {
  options.modules.nix = {
    bigHost = lib.mkOption {
      description = ''
        Host has the RAM and cores to spend on bigger download buffers and
        multi-threaded evaluation.
      '';
      type = lib.types.bool;
      default = false;
    };
    substituters.enable = lib.mkOption {
      description = ''
        Wire the shared substituter baseline. Only hosts that build closures
        need it — hyperion builds deploys, vulcan and prometheus build the CI
        fan-outs; deploy targets receive closures over SSH and never
        substitute.
      '';
      type = lib.types.bool;
      default = false;
    };
  };

  config = {
    # The generated NixOS manual builds an options.json that embeds custom
    # modules' declaration paths — i.e. the flake `-source` store path — "without
    # a proper context". That leaks a fetch-method-dependent path into every
    # host's toplevel, making it non-reproducible and unsharable via the binary
    # cache. Dropping the manual removes the leak and trims the closure.
    # optionalAttrs (not mkIf): documentation.nixos doesn't exist on Darwin, so
    # the key must be absent there rather than merely disabled.
    documentation = lib.optionalAttrs isLinux {
      nixos.enable = false;
    };

    nix = {
      gc = lib.mkIf (config.nix.enable || isLinux) ({
          automatic = true;
          options = "--delete-older-than 7d";
        }
        // lib.optionalAttrs isLinux {
          dates = "weekly";
        });

      settings = let
        users = ["root" config.user.name];
      in
        {
          trusted-users = users;
          allowed-users = users;
          auto-optimise-store = true;
          # 1GB for high-memory systems, 100MB for others (Raspberry Pis, etc.)
          download-buffer-size =
            if cfg.bigHost
            then 1000000000 # 1GB
            else 104857600; # 100MB

          experimental-features =
            ["nix-command" "flakes" "pipe-operators" "ca-derivations"]
            ++ lib.optional cfg.bigHost "parallel-eval";
        }
        // lib.optionalAttrs cfg.bigHost {
          # Multi-threaded evaluation, a Determinate Nix-only setting; 0 = all
          # cores (capped at 32). Kept off the Pis (not enough RAM) and janus
          # (stock Nix, which would warn on the unknown setting).
          eval-cores = 0;
        }
        // lib.optionalAttrs cfg.substituters.enable {
          # One union list for every build host, deliberately coarse: any of
          # them may build any host's closure (hyperion for deploys, the CI
          # runners for whole-fleet fan-outs), and per-host precision is
          # exactly the copy-paste drift this replaces. The one refinement is
          # dropping the host's own harmonia — querying your own store over
          # HTTP is a pure wasted roundtrip.
          substituters =
            lib.filter
            (s: !(lib.hasInfix config.networking.hostName s))
            [
              "https://install.determinate.systems"
              "https://nix-community.cachix.org"
              "https://nixos-raspberrypi.cachix.org"
              "https://niri.cachix.org"
              "http://vulcan.lan:4965"
              "http://prometheus.lan:4965"
            ];
          trusted-public-keys = [
            "cache.flakehub.com-3:hJuILl5sVK4iKm86JzgdXW12Y2Hwd5G07qKtHTOcDCM="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
            "niri.cachix.org-1:Wv0OmO7PsuocRKzfDoJ3mulSl7Z6oezYhGhR+3W2964="
            "vulcan.lan-1:Zu8N+6EtaIeDTyCVpR15uvIYYByZqMmd8W09vu8GKl8="
            "prometheus.lan-1:GetZTCVHg6NcVVteshbEZQbyMzZfIATcsIgt7si5Lmo="
          ];
        };
    };
  };
}
