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

    # TODO make more generic i.e. not zsh specific
    programs.zsh.shellAliases = {
      ls = "${pkgs.eza}/bin/eza";
      l = "${pkgs.eza}/bin/eza -lah";
      ll = "${pkgs.eza}/bin/eza -l";
      la = "${pkgs.eza}/bin/eza -a";
      lt = "${pkgs.eza}/bin/eza --tree";
      lla = "${pkgs.eza}/bin/eza -la";
    };
  };
}
