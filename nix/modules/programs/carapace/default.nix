{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.carapace;
in {
  options.modules.programs.carapace = {
    enable = mkEnableOption false;
  };

  config = mkIf cfg.enable {
    home.programs.carapace = {
      enable = true;
      enableNushellIntegration = mkIf config.modules.programs.nushell.enable true;
    };
  };
}
