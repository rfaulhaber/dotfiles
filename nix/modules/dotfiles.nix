# a number of modules reference scripts in dotfiles/bin.
# this file sets that up correctly
{
  config,
  lib,
  pkgs,
  ...
}: let
  home = config.home;
in {
  config = {
    assertions = [
      {
        assertion = builtins.pathExists config.dotfiles.dir;
        message = "config.dotfiles.dir does not exist";
      }
    ];

    home.file.dotfiles = {
      source = config.dotfiles.dir;
      target = "${config.user.home}/.config/dotfiles";
      recursive = true;
    };

    # TODO allow writing of dotfiles from other places
    # like how home-manager and etc allows for writing files directly
  };
}
