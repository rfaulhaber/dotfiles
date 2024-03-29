{
  config,
  lib,
  pkgs,
  ...
}:
# NOTE: this module is a disaster and needs cleaned up
with lib; let
  cfg = config.modules.desktop.bspwm;
  videoDrivers = config.modules.desktop.videoDrivers;
  keybindings = import ./sxhkd.nix {inherit pkgs config;};
in {
  options.modules.desktop.bspwm = {
    enable = mkOption {
      default = false;
      type = types.bool;
      description = "Enable bspwm";
    };
    extraStartupPrograms = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "Extra programs to start upon launch.";
    };
    monitors = mkOption {
      description = "Name of monitors for bspwm.";
      type = types.listOf types.str;
      default = null;
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.monitors != null;
        meessage = "Property displayName cannot be null.";
      }
    ];

    services.xserver.windowManager.bspwm.enable = true;

    modules.desktop.lightdm = {
      enable = true;
      defaultSession = "none+bspwm";
    };

    home.xsession = {
      # NB: IN ORDER FOR ANY OF THIS TO WORK YOU NEED THIS SET!!
      # I WASTED MOST OF A SUNDAY TRYING TO FIGURE THIS OUT!!!
      # IT SURE WOULD HAVE BEEN GREAT TO KNOW THAT SOMEWHERE!!!!
      enable = true;
      windowManager.bspwm = let
        monitors = cfg.monitors;
        bspwmConfig = import ./bspwm.nix {inherit config lib monitors;};
      in {
        inherit (bspwmConfig) monitors settings rules;
        enable = true;
        startupPrograms =
          bspwmConfig.startupPrograms
          ++ cfg.extraStartupPrograms;
      };
    };

    home.services.sxhkd = {
      inherit keybindings;
      enable = true;
    };
  };
}
