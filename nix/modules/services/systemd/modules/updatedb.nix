{
  lib,
  pkgs,
}: {
  name = "updatedb";
  service = {
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
}
