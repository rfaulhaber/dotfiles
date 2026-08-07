{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}: {
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
        # Hosts with the RAM and cores to spend on bigger download buffers and
        # multi-threaded evaluation.
        bigHosts = ["hyperion" "atlas" "vulcan"];
        isBigHost = lib.elem config.networking.hostName bigHosts;
      in
        {
          trusted-users = users;
          allowed-users = users;
          auto-optimise-store = true;
          # 1GB for high-memory systems, 100MB for others (Raspberry Pis, etc.)
          download-buffer-size =
            if isBigHost
            then 1000000000 # 1GB
            else 104857600; # 100MB

          experimental-features =
            ["nix-command" "flakes" "pipe-operators" "ca-derivations"]
            ++ lib.optional isBigHost "parallel-eval";
        }
        // lib.optionalAttrs isBigHost {
          # Multi-threaded evaluation, a Determinate Nix-only setting; 0 = all
          # cores (capped at 32). Kept off the Pis (not enough RAM) and janus
          # (stock Nix, which would warn on the unknown setting).
          eval-cores = 0;
        };
    };
  };
}
