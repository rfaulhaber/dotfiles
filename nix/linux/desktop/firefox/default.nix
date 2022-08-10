{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.desktop.firefox;

  findFirefoxExecPath = pkg:
    if (hasPrefix "firefox-devedition" pkg.pname) then
      "firefox-devedition"
    else
      "firefox";
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
    environment.systemPackages = [ cfg.package ];

    # I use zsh, this should be more modular but it's not
    programs.zsh.shellAliases = {
      firefox = "${cfg.package}/bin/${findFirefoxExecPath cfg.package}";
    };
  };
}
