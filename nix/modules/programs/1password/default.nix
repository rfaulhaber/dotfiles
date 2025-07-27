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
  options.modules.programs._1password = {enable = mkEnableOption false;};
  config = mkIf cfg.enable {
    programs = {
      _1password.enable = true;

      _1password-gui =
        {
          enable = true;
        }
        // lib.optionalAttrs isLinux {
          polkitPolicyOwners = [config.user.name];
        };
    };
  };
}
