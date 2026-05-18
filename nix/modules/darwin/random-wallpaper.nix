{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkOption types optional;
  cfg = config.modules.darwin.random-wallpaper;
in {
  options.modules.darwin.random-wallpaper =
    lib.my.wallpaperCommonOptions
    // {
      interval = mkOption {
        type = types.int;
        description = "Interval in seconds between wallpaper changes.";
        default = 1800; # 30 minutes
      };
    };

  config = mkIf cfg.enable {
    launchd.user.agents.random-wallpaper = let
      scriptPath =
        builtins.readFile "${config.dotfiles.binDir}/random-wallpaper.nu"
        |> lib.my.writeNushellScriptBin pkgs "random-wallpaper";
      queryArgs = optional (cfg.query != "") cfg.query;
      perDisplayArgs = optional cfg.perDisplay "--per-display";
    in {
      serviceConfig = {
        Label = "com.user.random-wallpaper";
        ProgramArguments =
          [
            "${scriptPath}/bin/random-wallpaper"
            "--desktop"
            "darwin"
          ]
          ++ perDisplayArgs
          ++ lib.my.wallpaperTokenArgs cfg.token
          ++ queryArgs;
        StartInterval = cfg.interval;
        RunAtLoad = true;
        StandardOutPath = "/tmp/random-wallpaper.log";
        StandardErrorPath = "/tmp/random-wallpaper.err";
      };
    };
  };
}
