{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  # TODO clean this up!
  cfg = config.modules.services.systemd;
  mergeModules = builtins.foldl' (left: right: left // right) {};
in {
  options.modules.services.systemd = {
    enable = mkEnableOption false;
    modules = mkOption {
      description = "List of custom systemd modules to enable.";
      type = types.listOf types.attrs;
    };
  };

  config = mkIf (builtins.length cfg.modules > 0) {
    systemd = {
      services =
        mergeModules (builtins.map (mod: {"${mod.name}" = mod.service;}) (builtins.filter (attrsets.hasAttrByPath ["service"]) cfg.modules));
      timers =
        mergeModules (builtins.map (mod: {"${mod.name}" = mod.timer;}) (builtins.filter (attrsets.hasAttrByPath ["timer"]) cfg.modules));
      user.services =
        mergeModules (builtins.map (mod: {"${mod.name}" = mod.user.service;}) (builtins.filter (attrsets.hasAttrByPath ["user" "service"]) cfg.modules));
      user.timers =
        mergeModules (builtins.map (mod: {"${mod.name}" = mod.user.timer;}) (builtins.filter (attrsets.hasAttrByPath ["user" "timer"]) cfg.modules));
    };
  };
}
