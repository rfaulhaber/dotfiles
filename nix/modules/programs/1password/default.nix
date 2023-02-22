{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs._1password;
in {
  options.modules.programs._1password = {enable = mkEnableOption false;};
  config = mkIf cfg.enable {
    programs = {
      _1password.enable = true;

      _1password-gui = {
        enable = true;
        polkitPolicyOwners = [config.user.name];
      };
    };
  };
}
