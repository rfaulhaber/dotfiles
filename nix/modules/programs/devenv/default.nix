{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.programs.devenv;
in {
  options.modules.programs.devenv = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    home.programs.devenv = {
      enable = true;
      enableNushellIntegration = mkIf config.modules.programs.nushell.enable true;
    };
  };
}
