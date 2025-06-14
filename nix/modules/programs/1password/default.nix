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
    # Core 1Password CLI (works on all platforms)
    programs._1password.enable = true;

    # Platform-specific GUI configuration
    programs._1password-gui =
      {
        enable = true;
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        # Only add polkitPolicyOwners on Linux where the option exists
        polkitPolicyOwners = [config.user.name];
      };
  };
}
