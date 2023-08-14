{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.firefox;

  findFirefoxExecPath = pkg:
    if (hasPrefix "firefox-devedition" pkg.pname)
    then "firefox-devedition"
    else "firefox";

  firefoxAlias = {
    firefox = "${cfg.package}/bin/${findFirefoxExecPath cfg.package}";
  };
in {
  options.modules.desktop.firefox = {
    enable = mkEnableOption false;
    package = mkOption {
      type = types.package;
      description = "Firefox package to use.";
      default = pkgs.firefox-devedition-bin;
    };
    setDefaultBrowser = mkOption {
      type = types.bool;
      description = "If true, sets the 'browser' alias to this package.";
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [cfg.package];

    programs.zsh.shellAliases = mkIf config.modules.programs.zsh.enable firefoxAlias;

    home.programs.nushell.shellAliases = mkIf config.modules.programs.nushell.enable firefoxAlias;
  };
}
