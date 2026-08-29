{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.modules.desktop.firefox;
  # The HiDPI bump firefox needs on 4K/scale-1 outputs, scoped to firefox's
  # own launcher instead of exported session-wide, where every XWayland app
  # with a GTK-reading toolkit inherited it (recipe: notes/hidpi_scaling.org).
  # overrideAttrs keeps .override intact, which programs.firefox calls
  # unconditionally on the package.
  hidpiPackage = cfg.package.overrideAttrs (old: {
    makeWrapperArgs =
      (old.makeWrapperArgs or [])
      ++ ["--set" "GDK_SCALE" "3" "--set" "GDK_DPI_SCALE" "1.5"];
  });
  firefoxAlias = {
    firefox = lib.getExe hidpiPackage;
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
    programs.firefox = {
      enable = true;
      package = hidpiPackage;
    };

    home.programs.nushell.shellAliases = lib.optionalAttrs config.modules.programs.nushell.enable firefoxAlias;

    environment.etc."xdg/mimeapps.list" = lib.mkIf cfg.setDefaultPDFViewer {
      text = ''
        [Default Applications]
        application/pdf=${cfg.package.pname}.desktop
      '';
    };
  };
}
