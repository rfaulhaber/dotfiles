{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.hyprland;
in {
  options.modules.desktop.environment.hyprland = {
    enable = mkEnableOption false;
    extraStartupPrograms = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "Extra programs to start upon launch.";
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.wayland.enable = true;
    modules.desktop.environment.type = "wayland";

    security.polkit.enable = true;

    programs = {
      hyprland = {
        enable = true;
        # we use the flake package for hyprland
        package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
        xwayland.enable = true;
      };
      waybar.enable = true;
    };

    # TODO avoid
    # TODO use gdm or regreet (https://github.com/rharish101/ReGreet)
    # services.xserver.displayManager.lightdm.enable = false;

    # services.greetd = {
    #   enable = true;
    #   settings = {
    #     default_session = {
    #       command = "${pkgs.greetd.regreet}/bin/regreet";
    #     };
    #   };
    # };

    # TODO write configuration defaults to a file, import in config

    # TODO can I make this a function?
    home.file.hyprconf = {
      source = "${config.dotfiles.configDir}/hypr/hyprland.conf";
      target = "${config.user.home}/.config/hypr/hyprland.conf";
    };


    environment.sessionVariables = {
      # required to fix issue where mouse is invisible
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    environment.systemPackages = with pkgs; [
      wofi
    ];
  };
}
