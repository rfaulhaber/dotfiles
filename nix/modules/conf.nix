{
  config,
  lib,
  pkgs,
  isLinux,
  isDarwin,
  ...
}:
with lib; {
  config = {
    nix = {
      gc =
        mkIf (config.nix.enable || isLinux) {
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
        download-buffer-size = 1000000000; # 1GB, hyperion has 64GB RAM

        experimental-features = ["nix-command" "flakes" "pipe-operators"];
      };
    };
  };
}
