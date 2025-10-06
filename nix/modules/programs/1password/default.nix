{
  config,
  lib,
  pkgs,
  isLinux,
  isDarwin,
  ...
}:
with lib; let
  cfg = config.modules.programs._1password;
in {
  options.modules.programs._1password = {
    enable = mkEnableOption false;
    beta = mkOption {
      type = types.bool;
      default = false;
      description = "If true, uses 1password beta.";
    };
    autostart = mkOption {
      type = types.bool;
      default = false;
      description = "(Linux only) If true, will autostart 1password gui.";
    };
  };
  config = mkIf cfg.enable {
    programs = {
      _1password.enable = true;

      _1password-gui =
        {
          enable = true;
        }
        // lib.optionalAttrs isLinux {
          polkitPolicyOwners = [config.user.name];
        }
        // lib.optionalAttrs cfg.beta {
          package = pkgs._1password-gui-beta;
        };
    };
    modules = lib.optionalAttrs isLinux {
      desktop.autostart.entries = lib.optionals cfg.autostart [
        "${pkgs._1password-gui}/share/applications/1password.desktop"
      ];
    };
  };
}
