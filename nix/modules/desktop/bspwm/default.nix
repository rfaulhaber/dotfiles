{ config, lib, pkgs, home-manager, ... }:

with lib;

let
  cfg = config.modules.desktop.bspwm;
  sxhkdKeybindings = import ./sxhkd.nix;
  bspwmConfig = import ./bspwm.nix;
in {
  options.modules.desktop.bspwm = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Enable bspwm";
    };
    extraStartupPrograms = mkOption {
      default = [ ];
      type = types.listOf types.str;
      description = "Extra programs to start upon launch.";
    };
  };
  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      windowManager.bspwm.enable = true;
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
        sessionCommands = ''
          ${config.dotfiles.binDir}/random-wallpaper
        '';
      };
      # TODO modularize!
      videoDrivers = [ "nvidia" ];
    };
    # NB: IN ORDER FOR ANY OF THIS TO WORK YOU NEED THIS SET!!
    # I WASTED MOST OF A SUNDAY TRYING TO FIGURE THIS OUT!!!
    # IT SURE WOULD HAVE BEEN GREAT TO KNOW THAT SOMEWHERE!!!!
    home-manager.users.${config.user.name}.xsession.enable = true;

    home.bspwm = {
      enable = true;
      inherit (bspwmConfig) monitors settings rules;
      startupPrograms = bspwmConfig.startupPrograms ++ cfg.extraStartupPrograms;
    };

    home.services.sxhkd = {
      enable = true;

      keybindings = sxhkdKeybindings;
    };
  };
}
