{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.hyprland;
  hyprConfig = import ./config.nix {inherit config lib pkgs;};
in {
  imports = [
    ./hyprconf.nix
  ];

  options.modules.desktop.environment.hyprland = {
    enable = mkEnableOption false;
    extraStartupPrograms = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "Extra programs to start upon launch.";
    };
  };

  config = mkIf cfg.enable {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    services.xserver.displayManager.gdm = {
      enable = true;
      wayland = true;
    };

    security.polkit.enable = true;

    environment.sessionVariables = {
      # required to fix issue where mouse is invisible
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    modules.desktop.hyprland.hyprconf = let
      bindings = hyprConfig.bindings;
      settings = hyprConfig.settings;
      extraConfig = hyprConfig.extraConfig;
    in {
      inherit bindings settings extraConfig;
    };

    environment.systemPackages = with pkgs; [
      wofi
    ];

    # TODO I would prefer to use home-manager, but there's something strange
    # about this setup that doens't work right

    # see: https://github.com/nix-community/home-manager/blob/master/modules/services/window-managers/hyprland.nix
    # home.wayland.windowManager.hyprland = {
    #   enable = true;
    #   enableNvidiaPatches = true;
    #   systemdIntegration = true;
    #   xwayland.enable = true;
    #   settings = let
    #     emacsclientExec = "${pkgs.emacs}/bin/emacsclient";
    #     firefoxExec = "${pkgs.firefox-devedition-bin}/bin/firefox-devedition";
    #   in {
    #     "$mod" = "SUPER";

    #     bind = [
    #       "$mod, enter, exec, kitty"
    #       "$mod, e, exec, ${emacsclientExec}"
    #       "$mod, b, exec, firefox"
    #       "$mod ALT, s, execr ${pkgs.gnome.gnome-screenshot}/bin/gnome-screenshot -i"
    #       "$mod ALT, e, execr ${emacsclientExec} -e '(emacs-everywhere)"
    #     ];
    #   };
    # };
  };
}
