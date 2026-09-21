{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.programs.gh;
in {
  options.modules.programs.gh = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    home.programs.gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };
  };
}
