{ config, lib, pkgs, home-manager, ... }:

with lib;

let
  cfg = config.modules.desktop.bspwm;
  keybindings = import ./sxhkd.nix { inherit pkgs config; };
  bspwmConfig = import ./bspwm.nix { inherit config; };
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
    # TODO put this configuration elsewhere
    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";
      windowManager.bspwm.enable = true;
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
      };
      # TODO modularize! not every comptuer will have an nvidia video card!
      videoDrivers = [ "nvidia" ];
    };
    # NB: IN ORDER FOR ANY OF THIS TO WORK YOU NEED THIS SET!!
    # I WASTED MOST OF A SUNDAY TRYING TO FIGURE THIS OUT!!!
    # IT SURE WOULD HAVE BEEN GREAT TO KNOW THAT SOMEWHERE!!!!
    home-manager.users.${config.user.name}.xsession.enable = true;

    home.bspwm = {
      inherit (bspwmConfig) monitors settings rules;
      enable = true;
      startupPrograms = bspwmConfig.startupPrograms ++ cfg.extraStartupPrograms;
    };

    home.services.sxhkd = {
      inherit keybindings;
      enable = true;
    };
  };
}
