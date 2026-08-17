{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.desktop.noctalia;

  noctaliaSettings = import ./config.nix {
    homePath = config.user.home;
    inherit (config.modules.themes) font;
  };
in {
  options.modules.desktop.noctalia = {enable = mkEnableOption false;};

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.desktop.environment.isWayland;
        message = "Must use noctalia with a wayland desktop";
      }
      {
        assertion = !config.modules.desktop.waybar.enable;
        message = "noctalia and waybar both claim the bar; enable only one";
      }
    ];

    nixpkgs.overlays = [inputs.noctalia.overlays.default];

    # Deliberately the home-manager module rather than nixosModules.default:
    # only this one exposes `settings`, and enabling both would define two
    # systemd user units for the same process.
    home-manager.users.${config.user.name} = {
      imports = [inputs.noctalia.homeModules.default];

      programs.noctalia = {
        enable = true;
        package = pkgs.noctalia;
        systemd.enable = true;
        settings = noctaliaSettings;
      };
    };
  };
}
