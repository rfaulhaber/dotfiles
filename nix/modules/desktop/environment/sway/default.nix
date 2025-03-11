{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.sway;
in {
  imports = [
    ../../wayland
  ];
  options.modules.desktop.environment.sway = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules.desktop.wayland.enable = true;
    modules.desktop.environment.type = "wayland";

    security.polkit.enable = true;

    # TODO swaylock
    # TODO waybar

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    home = {
      wayland.windowManager.sway = {
        enable = true;
        config = {
          modifier = "Mod4";
          terminal = "wezterm"; # TODO figure out automatically
          wrapperFeatures.gtk = true;
          keybindings = mkOptionDefault {
            "$mod+Return" = "exec ${pkgs.kitty}/bin/wezterm";
            "$mod+b" = "exec ${pkgs.firefox-devedition-bin}/bin/firefox-developer-edition";
            "$mod+e" = "exec emacsclient -c";
          };
        };
      };

      programs = {
        fuzzel = {
          enable = true;
          settings = {
            main = {
              terminal = "${pkgs.kitty}/bin/wezterm";
            };
          };
        };
        swaylock.enable = true;
      };
    };

    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.greetd}/bin/agreety --cmd ${pkgs.sway}/bin/sway";
        };
      };
    };

    hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.latest;

    environment.systemPackages = with pkgs; [
      inputs.swww.packages.${pkgs.system}.swww
    ];
  };
}
