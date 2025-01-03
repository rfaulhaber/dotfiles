{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.hyprland;
  primaryMonitor = config.modules.desktop.monitors;
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

    nix.settings = {
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };

    programs = {
      hyprland = {
        enable = true;
        # we use the flake package for hyprland
        package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
        portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
        xwayland.enable = true;
      };
      waybar.enable = true;
    };

    # home.wayland.windowManager.hyprland = {
    #   enable = true;
    #   package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    #   xwayland.enable = true;
    # };

    # TODO avoid, I'm not sure why this is necessary
    services.xserver.displayManager.lightdm.enable = false;

    # TODO use gdm or regreet (https://github.com/rharish101/ReGreet)
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
      GBM_BACKEND = "nvidia-drm";
      LIBVA_DRIVER_NAME = "nvidia"; # hardware acceleration
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      NIXOS_OZONE_WL = "1";
    };

    environment.systemPackages = with pkgs; [
      fuzzel
    ];
  };
}
