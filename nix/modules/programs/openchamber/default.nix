{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.modules.programs.openchamber;
  inherit (pkgs.stdenv.hostPlatform) system;
  packages = inputs.openchamber.packages.${system};
in {
  options.modules.programs.openchamber = {
    desktop.enable = mkEnableOption "the OpenChamber desktop app";
    cli.enable = mkEnableOption "the `openchamber` CLI, which serves the OpenChamber web UI";
  };

  config = mkIf (cfg.desktop.enable || cfg.cli.enable) {
    assertions = [
      {
        assertion = inputs.openchamber.packages ? ${system};
        message = "OpenChamber's flake provides no packages for ${system}.";
      }
    ];

    user.packages =
      optional cfg.desktop.enable packages.openchamber-desktop
      ++ optional cfg.cli.enable packages.openchamber;
  };
}
