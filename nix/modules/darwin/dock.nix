{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.darwin.dock;
in {
  options.modules.darwin.dock = {enable = mkEnableOption false;};

  # TODO make work on more than one system
  config = mkIf cfg.enable {
    system.defaults.dock = {
      show-recents = false;
      autohide = true;
      persistent-apps = [
        {
          app = "/Applications/Firefox.app";
        }
        {
          app = "/Users/ryan/Applications/Emacs.app";
        }
        (
          lib.optionalAttrs (config.modules.programs.wezterm.enable) {
            app = "${config.modules.programs.wezterm.weztermPackage}/Applications/WezTerm.app";
          }
        )
        {
          app = "/Applications/Qobuz.app";
        }
        {
          app = "/Applications/Signal.app";
        }
        {
          app = "/System/Applications/Weather.app";
        }
        {
          app = "/System/Applications/System Settings.app";
        }
      ];
      persistent-others = [
        "/Applications"
        "/Users/ryan/Downloads"
      ];
    };
  };
}
