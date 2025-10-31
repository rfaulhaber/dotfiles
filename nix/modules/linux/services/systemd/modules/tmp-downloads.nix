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
      nuExec = "${pkgs.nushell}/bin/nu";
      scriptPath =
        builtins.readFile "${config.dotfiles.binDir}/tmp-downloads.nu"
        |> pkgs.writeScriptBin "tmp-downloads";
      cmd = "${scriptPath}/bin/tmp-downloads --link ${cfg.targetDir}";
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
