{
  config,
  lib,
  pkgs,
  ...
}:
# NOTE: this module is a disaster and needs cleaned up
with lib; let
  cfg = config.modules.desktop.environment.bspwm;
  desktop = config.modules.desktop;
  monitors = desktop.monitors;
  keybindings = import ./sxhkd.nix {inherit pkgs config;};
in {
  imports = [
    ../../xserver
  ];
  options.modules.desktop.environment.bspwm = {
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
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (length monitors) > 0;
        meessage = "Must provide at least one monitor";
      }
    ];

    # enables custom module: modules.desktop.environment.xserver
    # and common xserver configs
    # TODO set by default
    modules.desktop.xserver.enable = true;
    modules.desktop.environment.type = "x11";

    services.xserver.windowManager.bspwm.enable = true;

    modules.desktop = {
      lightdm = {
        enable = true;
        defaultSession = "none+bspwm";
      };
      polybar.enable = true;
      rofi.enable = true;
    };

    home.xsession = {
      # NOTE: the following angry comment was written when I was just starting
      # to use NixOS, and I did not realize that it was convention to explicitly
      # enable modules. I now know better. I have left this comment in for
      # amusement and historical purposes:

      # NB: IN ORDER FOR ANY OF THIS TO WORK YOU NEED THIS SET!!
      # I WASTED MOST OF A SUNDAY TRYING TO FIGURE THIS OUT!!!
      # IT SURE WOULD HAVE BEEN GREAT TO KNOW THAT SOMEWHERE!!!!
      enable = true;
      windowManager.bspwm = let
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
