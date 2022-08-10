{ config, lib, pkgs, ... }:

with lib;

let cfg = config.common.themes;
in {
  options.common.themes = {
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
