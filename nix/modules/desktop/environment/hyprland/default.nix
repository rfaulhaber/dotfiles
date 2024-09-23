{
  config,
  lib,
  pkgs,
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

    security.polkit.enable = true;

    programs = {
      hyprland.enable = true;
      waybar.enable = true;
    };

    # TODO avoid
    services.xserver.displayManager.lightdm.enable = false;

    # services.greetd = {
    #   enable = true;
    #   settings = {
    #     default_session = {
    #       command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd hyprland";
    #     };
    #   };
    # };

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
