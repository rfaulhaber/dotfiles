{ config, lib, pkgs, ... }:

{
  user = rec {
    name = "ryan";
    home = "/Users/${name}";
  };

  services.nix-daemon.enable = true;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };
}
