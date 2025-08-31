{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./preferences.nix
    ./dock.nix
  ];
  config = {
    system.primaryUser = config.user.name;
  };
}
