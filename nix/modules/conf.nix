{
  config,
  lib,
  pkgs,
  isLinux,
  isDarwin,
  ...
}: {
  config = {
    nix = {
      gc =
        lib.mkIf (config.nix.enable || isLinux) {
          automatic = true;
          options = "--delete-older-than 7d";
        }
        // lib.optionalAttrs isLinux {
          dates = "weekly";
        };

      settings = let
        users = ["root" config.user.name];
      in {
        trusted-users = users;
        allowed-users = users;
        auto-optimise-store = true;
        # 1GB for high-memory systems, 100MB for others (Raspberry Pis, etc.)
        download-buffer-size =
          if config.networking.hostName == "hyperion" || config.networking.hostName == "atlas"
          then 1000000000 # 1GB
          else 104857600; # 100MB

        experimental-features = ["nix-command" "flakes" "pipe-operators"];
      };
    };
  };
}
