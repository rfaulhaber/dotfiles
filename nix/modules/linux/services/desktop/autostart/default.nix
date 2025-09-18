{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.autostart;
in {
  options.modules.desktop.autostart = {
    enable = mkEnableOption false;
    entries = mkOption {
      type = types.listOf types.str;
      description = "List of programs to autostart";
      default = [];
    };
  };

  config = mkIf cfg.enable {
    assertions =
      [
        {
          assertion = config.modules.desktop.enable;
          message = "autostart module must be used with the desktop module.";
        }
      ]
      ++ (builtins.map (p: {
          assertion = builtins.pathExists p;
          message = "path should exist: ${p}";
        })
        cfg.entries);

    home.autostart = {
      inherit (cfg) entries;
      enable = true;
      readOnly = true;
    };
  };
}
