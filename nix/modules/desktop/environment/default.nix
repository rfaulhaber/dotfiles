{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.desktop.environment;
in {
  imports = [
    ./awesome
    ./bspwm
    ./hyprland
    ./i3
    ./niri
    ./retroarch
    ./sway
  ];

  options.modules.desktop.environment = {
    type = mkOption {
      description = "Desktop environment type.";
      type = types.enum ["x11" "wayland" "none"];
      default = "none";
    };
    isX11 = mkOption {
      description = "If true, `type` is an X11 desktop.";
      type = types.bool;
      default = cfg.type == "x11";
    };
    isWayland = mkOption {
      description = "If true, `type` is a Wayland desktop.";
      type = types.bool;
      default = cfg.type == "wayland";
    };
  };

  config = {
    assertions = [
      {
        assertion = !(cfg.isX11 && cfg.isWayland);
        message = "You cannot have an x11 and wayland environment enabled at the same time.";
      }
    ];
  };
}
