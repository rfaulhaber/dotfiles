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

        experimental-features = ["nix-command" "flakes" "pipe-operators"];
      };
    };
  };
}
