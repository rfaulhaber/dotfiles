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
      nuExec = "${pkgs.nushell}/bin/nu";
      scriptPath = "${config.dotfiles.binDir}/tmp-downloads.nu";
      baseCmd = "${nuExec} ${scriptPath}";
      cmd = "${baseCmd} --target ${userRunDir} --link ${cfg.targetDir}";
    in {
      path = [pkgs.nushell];
      after = ["local-fs.target"];
      wants = ["local-fs.target"];
      wantedBy = ["default.target"];
      description = "Sets up temporary Downloads directory in tmpfs.";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart = cmd;
      };
    };
  };
}
