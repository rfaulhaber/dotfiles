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
  };
}
