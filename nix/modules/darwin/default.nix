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

    # see note in nushell. nix doesn't set nushell variables correctly
    environment.variables = lib.optionalAttrs (config.modules.programs.nushell.enable) {
      SHELL = "/run/current-system/sw/bin/nu";
    };
  };
}
