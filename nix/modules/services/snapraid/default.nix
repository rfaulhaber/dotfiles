{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.snapraid;
in {
  # these options mostly forward to the snapraid module
  options.modules.services.snapraid = {
    enable = mkEnableOption false;
    dataDisks = mkOption {
      type = types.attrsOf types.str;
      description = "Data disks.";
    };
    parityFiles = mkOption {
      type = types.listOf types.str;
      description = "List of parity files.";
    };
    contentFiles = mkOption {
      type = types.listOf types.str;
      description = "Content files.";
    };
  };

  config = mkIf cfg.enable {
    snapraid = {
      enable = true;
      dataDisks = cfg.dataDisks;
      parityFiles = cfg.parityFiles;
      contentFiles = cfg.contentFiles;
      exclude = [ "/tmp/" "/lost+found/" ];
    };
  };
}
