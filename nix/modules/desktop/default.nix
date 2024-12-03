{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
in {
  imports = [
    ./environment
    ./firefox
    ./lightdm
    ./polybar
    ./random-wallpaper.nix
    ./rofi
    ./sound
  ];
  options.modules.desktop = {
    # TODO rewrite such that you don't need this
    enable = mkEnableOption false;
    monitors = mkOption {
      description = "List of monitors.";
      type = types.listOf types.str;
      default = [];
    };
    laptop = mkOption {
      description = "Laptop-specific settings.";
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            description = "Enables laptop settings.";
            default = false;
          };
        };
      };
      default = {enable = false;};
    };
    extraPackages = mkOption {
      description = "List of extra packages to install for the default user.";
      type = types.listOf types.package;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    assertions = let
      foldPred = acc: item:
      # TODO probably a more elegant way to do this
        if (item.name != "type" && item.value.enable)
        then acc ++ [item.name]
        else acc;
      desktopsEnabled = pipe config.modules.desktop.environment [attrsToList (foldl foldPred [])];
    in [
      {
        assertion = (length desktopsEnabled) == 1;
        message = "You must have one desktop environment selected if the desktop module is enabled. You have ${toString (length desktopsEnabled)} (${toString desktopsEnabled})";
      }
      {
        assertion = cfg.environment.type != "none";
        message = "Desktop type cannot be 'none' if a desktop is enabled.";
      }
    ];

    # TODO make fonts module
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs;
        [
          lato
          merriweather
        ]
        ++ (with pkgs.nerd-fonts; [
          hack
        ]);
      fontconfig.defaultFonts = {
        serif = ["Merriweather"];
        sansSerif = ["Lato"];
        monospace = ["Hack Nerd Font Mono"];
      };
    };

    # despite the name, this is set for either X or wayland desktops
    services.xserver = {
      enable = true;
      videoDrivers = mkIf config.modules.hardware.nvidia.enable ["nvidia"];
      xkb = {
        options = "eurosign:e";
        layout = "us";
      };
    };

    # sets desktop to dark theme
    programs.dconf.enable = true;
    home.dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };

    # TODO create pdf module
    programs.evince.enable = true;

    # TODO put these somewhere better
    # necessary utilities for desktop
    user.packages = cfg.extraPackages;
  };
}
