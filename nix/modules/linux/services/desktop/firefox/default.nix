{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.modules.desktop.firefox;
  firefoxAlias = {
    firefox = lib.getExe cfg.package;
  };
in {
  options.modules.desktop.firefox = {
    enable = lib.mkEnableOption false;
    package = lib.mkOption {
      type = lib.types.package;
      description = "Firefox package to use.";
      default = pkgs.firefox-devedition;
    };
    setDefaultPDFViewer = lib.mkOption {
      type = lib.types.bool;
      description = "If true, sets firefox to be the default PDF viewer";
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [cfg.package];

    programs.zsh.shellAliases = lib.optionalAttrs config.modules.programs.zsh.enable firefoxAlias;

    home.programs.nushell.shellAliases = lib.optionalAttrs config.modules.programs.nushell.enable firefoxAlias;

    environment.etc."xdg/mimeapps.list" = lib.mkIf cfg.setDefaultPDFViewer {
      text = ''
        [Default Applications]
        application/pdf=${cfg.package.pname}.desktop
      '';
    };
  };
}
