{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd.modules.updatedb;
in {
  options.modules.services.systemd.modules.updatedb = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    systemd.services.updatedb = {
      description = "Periodically runs updatedb.";
      after = ["multi-user.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        # since updatedb will most likely exit with a non-zero status code
        # (since it can't read most of the root directory), we'll ignore the
        # status code
        ExecStart = "-${pkgs.findutils}/bin/updatedb";
      };
    };

    environment.systemPackages = with pkgs; [findutils];
  };
}
