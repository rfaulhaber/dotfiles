{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.aspell;
in {
  options.modules.programs.aspell = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
    ];

    home.file.aspellconf = {
      text = ''
        data-dir /etc/profiles/per-user/ryan/lib/aspell
      '';
      target = "${config.user.home}/.aspell.conf";
    };
  };
}
