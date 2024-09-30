{lib, ...}:
with lib; let
  cfg = config.modules.desktop.environment;
in {
  imports = [
    ./awesome
    ./bspwm
    ./hyprland
    ./i3
    ./sway
  ];

  options.modules.desktop.environment = {
    type = mkOption {
      description = "Desktop environment type.";
      type = types.enum ["x11" "wayland" "none"];
      default = "none";
    };
  };
}
