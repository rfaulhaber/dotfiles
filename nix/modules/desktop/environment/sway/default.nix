{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment.sway;
in {
  imports = [
    ./waybar.nix
    ../../wayland
  ];
  options.modules.desktop.environment.sway = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    modules.desktop.wayland.enable = true;
    # modules.desktop.environment.sway.waybar.enable = true;

    security.polkit.enable = true;

    # TODO swaylock
    # TODO waybar

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    home = {
      # wayland.windowManager.sway = {
      #   enable = true;
      #   config = {
      #     modifier = "Mod4";
      #     terminal = "kitty"; # TODO figure out automatically
      #     startup = [
      #       {command = "${pkgs.kitty}/bin/kitty";}
      #     ];
      #     systemd.enable = true;
      #     wrapperFeatures.gtk = true;
      #     keybindings = mkOptionDefault {
      #       "$mod+Return" = "exec ${pkgs.kitty}/bin/kitty";
      #       "$mod+b" = "exec ${pkgs.firefox-devedition-bin}/bin/firefox-developer-edition";
      #     };
      #   };
      # };

      programs = {
        fuzzel = {
          enable = true;
          settings = {
            main = {
              terminal = "${pkgs.kitty}/bin/kitty";
            };
          };
        };
        swaylock = {
          enable = true;
        };
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

    environment.systemPackages = with pkgs; [
      # grim # screenshot functionality
      # slurp # screenshot functionality
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    ];
  };
}
