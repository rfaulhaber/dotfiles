{ config, lib, pkgs, home-manager, ... }:

with lib;

let
  cfg = config.modules.desktop.polybar;
  colors = config.modules.themes.colors;
  modules = (import ./modules) { inherit colors; };
in {
  options.modules.desktop.polybar = {
    enable = mkOption {
      default = false;
      description = "Enable polybar";
      type = types.bool;
      example = true;
    };
  };
  config = mkIf cfg.enable {
    home.services.polybar = {
      enable = true;
      package = pkgs.polybarFull;
      script = ''
        # Terminate already running bar instances
        killall polybar

        # Wait until the processes have been shut down
        while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

        # Launch polybar
        polybar main &
      '';
      config = {
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
          font-0 = "Hack Nerd Font:size=16;3";
          font-1 = "Hack Nerd Font:size=16;3";
          font-2 = "Hack Nerd Font:size=16;3";
          font-3 = "Hack Nerd Font:size=16;3";

          line-size = 3;
          line-color = "#f00";

          # modules
          modules-left = "bspwm xwindow";
          modules-right = "wttr cpu memory pulseaudio date";
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

        "settings" = { screenchange-reload = true; };

        "global/wm" = {
          margin-top = 5;
          margin-bottom = 5;
        };

        "module/bspwm" = modules.bspwm;
        "module/date" = modules.date;
        "module/wttr" = modules.wttr;
        "module/cpu" = modules.cpu;
        "module/memory" = modules.memory;
        "module/pulseaudio" = modules.pulseaudio;
      };
    };
  };
}
