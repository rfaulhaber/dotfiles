{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.hyprland.hyprconf;
in {
  options.modules.desktop.hyprland.hyprconf = {
    bindings = mkOption {
      type = type.attrsOf (types.submodule {
        options = {
          keys = mkOption {
            type = types.listOf types.str;
            description = "List of keybindings.";
          };
          mouse = mkOption {
            type = types.listOf types.str;
            description = "List of mouse bindings.";
          };
        };
      });
    };
    settings = mkOption {
      type = type.attrs;
      description = "Regular Hypr configuration.";
    };
    extraConfig = mkOption {
      type = types.str;
      description = "Additional literal config added to the end of Hyprland config file.";
    };
  };

  config = {
    # TODO fill me out!
    home.xdg.configFile."hypr/hyprland.conf" = "hello world";
  };
}
