{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.polybar;
  desktopCfg = config.modules.desktop;
  colors = config.modules.themes.colors;
  modules = (import ./modules) {inherit colors config pkgs;};
in {
  options.modules.desktop.polybar = {
    enable = mkOption {
      default = false;
      description = "Enable Polybar";
      type = types.bool;
      example = true;
    };
    fontSize = mkOption {
      default = 16;
      description = "Polybar font size.";
      type = types.int;
      example = 16;
    };
  };
  config = mkIf cfg.enable {
    home.services.polybar = {
      enable = true;
      package = pkgs.polybarFull;
      # Polybar will be started by bspwm
      script = "";
      config = let
        fontSizeStr = toString cfg.fontSize;
      in {
        "bar/main" = {
          enable-ipc = true;
          background = "${colors.bg}";
          background-alt = "${colors.bg-alt}";
          foreground = "${colors.fg}";
          foreground-alt = "${colors.fg-alt}";

          # size
          width = "100%";
          height = 45;
          bottom = false;
          fixed-center = true;

          # fonts
          font-0 = "Hack Nerd Font:size=${fontSizeStr};3";
          font-1 = "Hack Nerd Font:size=${fontSizeStr};3";
          font-2 = "Hack Nerd Font:size=${fontSizeStr};3";
          font-3 = "Hack Nerd Font:size=${fontSizeStr};3";

          line-size = 3;
          line-color = "#f00";

          # modules
          modules-left = "bspwm xwindow";
          modules-right = let
            defaultRightModules = [
              "wttr"
              "cpu"
              "memory"
              "pulseaudio"
              (
                if desktopCfg.laptop.enable
                then "battery"
                else ""
              )
              "date"
            ];
          in
            concatStringsSep " " defaultRightModules;
          module-margin = 0;

          # tray
          tray-position = "right";
          tray-padding = 12;

          cursor-click = "pointer";
          cursor-scroll = "ns-resize";

          # misc
          wm-restack = "bspwm";
        };

        "module/xwindow" = {
          type = "internal/xwindow";
          label = "%title:0:30:...%";
        };

        "settings" = {screenchange-reload = true;};

        "global/wm" = {
          margin-top = 5;
          margin-bottom = 5;
        };

        "module/bspwm" = modules.bspwm;
        "module/date" = modules.date;
        "module/wttr" = modules.wttr;
        "module/cpu" = modules.cpu;
        "module/battery" = modules.battery;
        "module/memory" = modules.memory;
        "module/pulseaudio" = modules.pulseaudio;
      };
    };
  };
}
