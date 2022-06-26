{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.themes;
in {
  options.modules.themes = {
    active = mkOption {
      type = types.str;
      description = "The active theme.";
    };

    colors = mkOption {
      type = types.attrs;
      description = "Active color set.";
      # TODO this makes the color palette generally available, but is there a
      # better way to do this?
      apply = v: import (./. + "/${cfg.active}.nix");
    };
  };
}
