# the home manager exa package doesn't set the aliases correctly for some
# reason, so I just recreated their package
{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.programs.exa;
in {
  options.modules.programs.exa = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [exa];

    # TODO make more generic i.e. not zsh specific
    programs.zsh.shellAliases = {
      ls = "${pkgs.exa}/bin/exa";
      l = "${pkgs.exa}/bin/exa -lah";
      ll = "${pkgs.exa}/bin/exa -l";
      la = "${pkgs.exa}/bin/exa -a";
      lt = "${pkgs.exa}/bin/exa --tree";
      lla = "${pkgs.exa}/bin/exa -la";
    };
  };
}
