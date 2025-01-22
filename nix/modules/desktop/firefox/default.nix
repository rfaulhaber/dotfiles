{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.firefox;

  isDevEdition = pkg: hasPrefix "firefox-devedition" pkg.pname;
  findFirefoxExecPath = pkg:
    if isDevEdition pkg
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
    setDefaultPDFViewer = mkOption {
      type = types.bool;
      description = "If true, sets firefox to be the default PDF viewer";
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [cfg.package];

    programs.zsh.shellAliases = mkIf config.modules.programs.zsh.enable firefoxAlias;

    home.programs.nushell.shellAliases = mkIf config.modules.programs.nushell.enable firefoxAlias;

    environment.etc."xdg/mimeapps.list" = mkIf cfg.setDefaultPDFViewer {
      text = let
        xdgName =
          if isDevEdition cfg.package
          then "firefox-developer-edition"
          else "firefox";
      in ''
        [Default Applications]
        application/pdf=${xdgName}.desktop
      '';
    };
  };
}
