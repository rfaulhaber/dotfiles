{ config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.services.systemd;
in {
  # TODO refactor as constant
  config = mkIf (cfg.enable && (builtins.elem "updatedb" cfg.modules)) {
    systemd.services.updatedb = {
      description = "Periodically runs updatedb.";
      after = [ "multi-user.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        # since updatedb will most likely exit with a non-zero status code
        # (since it can't read most of the root directory), we'll ignore the
        # status code
        ExecStart = "-${pkgs.findutils}/bin/updatedb";
      };
    };
  };
}
