# standard system configuration items across Linux systems for NixOS
#
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  config = mkIf pkgs.stdenv.targetPlatform.isLinux {
    # All my machines are in the same timezone
    # TODO should this be here?

    time.timeZone = "America/New_York";

    # TODO where should these live?
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    # TODO make standard nix module?
    nix = {
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
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

    system.stateVersion = "23.11";
  };
}
