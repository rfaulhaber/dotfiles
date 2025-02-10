{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.services.systemd.modules.tmp-downloads;
in {
  options.modules.services.systemd.modules.tmp-downloads = {
    enable = mkEnableOption false;
    targetDir = mkOption {
      description = "Where to place Downloads directory.";
      type = types.oneOf [types.path types.str];
      default = "/home/${config.user.name}/Downloads";
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.tmp-downloads = let
      inherit (config.user) name uid;
      userRunDir = "/run/user/${toString uid}/downloads";
    in {
      after = ["local-fs.target"];
      wants = ["local-fs.target"];
      wantedBy = ["default.target"];
      description = "Sets up temporary Downloads directory in tmpfs.";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir ${userRunDir}";
        ExecStart = "${pkgs.coreutils}/bin/ln -sf ${userRunDir} ${cfg.targetDir}";
        ExecStop = "${pkgs.coreutils}/bin/rm ${cfg.targetDir} && ${pkgs.coreutils}/bin/rm -rf ${userRunDir}";
      };
    };
  };
}
