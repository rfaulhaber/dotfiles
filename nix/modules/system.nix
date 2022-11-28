{ config, lib, pkgs, ... }:

with lib;

# standard system configuration items across Linux systems for NixOS

{
  config = {
    # All my machines are in the same timezone
    # TODO should this be here?
    time = {
      timeZone = "America/New_York";
      hardwareClockInLocalTime = true;
    };

    # TODO where should these live?
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

    # TODO make standard nix module?
    nix = {
      package = pkgs.nixVersions.stable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };

      settings = let users = [ "root" config.user.name ];
      in {
        trusted-users = users;
        allowed-users = users;
        auto-optimise-store = true;
      };
    };

    nixpkgs.config.allowUnfree = true;

    system.stateVersion = "22.11";
  };
}
