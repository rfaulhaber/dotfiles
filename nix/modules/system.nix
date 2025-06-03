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
      package = pkgs.nixVersions.stable;
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

        experimental-features = let
          lixEnabled = builtins.hasAttr "lix" config && config.lix.enable;
        in
          ["nix-command" "flakes"]
          ++ (
            # this is so silly, but cppnix's feature is called "pipe-operators"
            # while lix's feature is called "pipe-operator". not all my machines
            # use lix, so I have to account for this
            if lixEnabled
            then ["pipe-operator"]
            else ["pipe-operators"]
          );
      };
    };

    system.stateVersion = "23.11";
  };
}
