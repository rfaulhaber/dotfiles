{ config, lib, pkgs, home-manager, ... }:

with lib;

let cfg = config.modules.desktop.bspwm;
in {
  options.modules.desktop.bspwm = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Enable bspwm";
    };
  };
  config = mkIf cfg.enable {
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      windowManager = { bspwm = { enable = true; }; };
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
        sessionCommands = ''
          ~/Projects/dotfiles/nix/hosts/mir3/random-wallpaper.sh
        '';
      };
      videoDrivers = [ "nvidia" ];
    };
    xsession.windowManager.bspwm = {
      enable = true;
      monitors = {
        HDMI-0 = [ "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" ];
      };
      startupPrograms = [ "sxhkd" "$HOME/.config/bin/polybar/launch" ];
      settings = {
        border_width = 0;
        window_gap = 12;
        split_ratio = 0.52;
        borderless_monocle = true;
        gapless_monocle = true;
        external_rules_command = "$HOME/.config/bin/bspwm/external_rules";
      };
      rules = {
        "Firefox Developer Edition" = {
          desktop = "^1";
          state = "tiled";
          focus = true;
        };
        "Emacs" = {
          desktop = "^2";
          state = "tiled";
          focus = true;
          locked = true;
        };
        kitty = {
          desktop = "^3";
          state = "tiled";
          focus = true;
        };
        discord = {
          desktop = "^4";
          state = "tiled";
        };
        "TelegramDesktop" = {
          desktop = "^4";
          state = "tiled";
        };
      };
    };

  };
}
