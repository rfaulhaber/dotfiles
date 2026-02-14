{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.claude;
in {
  options.modules.programs.claude = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    home.programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;
    };
  };
}
