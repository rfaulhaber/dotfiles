{
  config,
  lib,
  pkgs,
  isLinux,
  ...
}:
with lib; let
  cfg = config.modules.programs.heroic;
in {
  options.modules.programs.heroic = {enable = mkEnableOption false;};

  config = mkIf cfg.enable ({
      user.packages = [pkgs.heroic];
    }
    # NixOS-only options. Defining them on darwin fails evaluation even under
    # a false mkIf, because the option itself is undeclared there.
    // optionalAttrs isLinux {
      programs = {
        gamescope.enable = true;
        gamemode.enable = true;
      };
    });
}
