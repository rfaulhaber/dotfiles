# the home manager eza package doesn't set the aliases correctly for some
# reason, so I just recreated their package
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.eza;
in {
  options.modules.programs.eza = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [eza];

    # TODO: Add shell aliases through home-manager for cross-platform compatibility
    # ZSH aliases removed temporarily due to Darwin compatibility issues
  };
}
