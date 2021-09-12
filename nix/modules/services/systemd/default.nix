{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.systemd;
in {
  imports = [ ./updatedb.nix ];
  options.modules.services.systemd = {
    modules = mkOption {
      description = "List of extra systemd modules to enable.";
      type = types.listOf types.str;
    };
  };
}
