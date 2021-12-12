{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.mergerfs;
in {
  options.modules.services.mergerfs = {
    enable = mkEnableOption false;
    # TODO support multiple configurations
    branches = mkOption {
      type = types.listOf types.str;
      description = "Branches used by mergerfs.";
      default = [ ];
    };
    target = mkOption {
      type = types.str;
      description = "Mount point.";
    };
  };

  config = mkIf cfg.enable {
    fileSystems."${cfg.target}" = {
      device = concatStringsSep ":" cfg.branches;
      fsType = "fuse.mergerfs";
      options = [
        "allow_other"
        "use_ino"
        "cache.files=partial"
        "dropcacheonclose=true"
        "category.create=mfs"
      ];
    };
  };
}
