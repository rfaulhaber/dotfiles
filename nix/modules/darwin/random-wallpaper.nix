{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.darwin.random-wallpaper;
in {
  options.modules.darwin.random-wallpaper = {
    enable = mkEnableOption "random wallpaper service";

    interval = mkOption {
      type = types.int;
      description = "Interval in seconds between wallpaper changes.";
      default = 1800; # 30 minutes
    };

    query = mkOption {
      type = types.str;
      description = "Optional query to pass to random wallpaper endpoint.";
      default = "";
    };

    token = mkOption {
      description = "API token for wallpaper API.";
      type = types.either types.str types.path;
    };
  };

  config = mkIf cfg.enable {
    launchd.user.agents.random-wallpaper = let
      scriptPath =
        builtins.readFile "${config.dotfiles.binDir}/random-wallpaper.nu"
        |> lib.my.writeNushellScriptBin "random-wallpaper";
      tokenArgs =
        if builtins.isPath cfg.token || lib.hasPrefix "/" cfg.token
        then ["--token-file" (toString cfg.token)]
        else ["--token" cfg.token];
      queryArgs =
        if cfg.query != ""
        then [cfg.query]
        else [];
    in {
      serviceConfig = {
        Label = "com.user.random-wallpaper";
        ProgramArguments =
          [
            "${scriptPath}/bin/random-wallpaper"
            "--desktop"
            "darwin"
          ]
          ++ tokenArgs
          ++ queryArgs;
        StartInterval = cfg.interval;
        RunAtLoad = true;
        StandardOutPath = "/tmp/random-wallpaper.log";
        StandardErrorPath = "/tmp/random-wallpaper.err";
      };
    };
  };
}
