{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.attrsets;
with builtins; let
  cfg = config.modules.services.systemd;
  mergeModules = foldl' (left: right: left // right) {};
  modFromPath = path: mod: {"${mod.name}" = getAttrFromPath path mod;};
  filterModsByPath = path: modules: filter (hasAttrByPath path) modules;
  mkModules = path: modules: mergeModules (map (modFromPath path) (filterModsByPath path modules));
in {
  options.modules.services.systemd = {
    enable = mkEnableOption false;
    modules = mkOption {
      description = "List of custom systemd modules to enable.";
      type = types.listOf types.attrs;
    };
  };

  config = mkIf (length cfg.modules > 0) {
    systemd = {
      services =
        mkModules ["service"] cfg.modules;
      timers =
        mkModules ["timers"] cfg.modules;
      user.services =
        mkModules ["user" "services"] cfg.modules;
      user.timers =
        mkModules ["user" "timers"] cfg.modules;
    };

    environment.systemPackages = with pkgs; [findutils];
  };
}
