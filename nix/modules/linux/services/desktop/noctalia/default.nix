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
    inherit (cfg) networkInterface;
    allowEmptyPassword = config.modules.services.yubikey.enable;
    inherit (config.userInfo) location;
    lockscreenOutputs = attrNames config.modules.desktop.environment.niri.outputs;
  };
in {
  options.modules.desktop.noctalia = {
    enable = mkEnableOption false;

    networkInterface = lib.my.mkOptDesc types.str "" ''
      Interface the bar's network throughput widgets sample. Left empty, sysmon
      sums every non-loopback interface, so an overlay like netbird's wt0 is
      counted alongside the physical link it already egresses over.
    '';
  };

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

    # home-manager ships programs.noctalia upstream now; importing
    # inputs.noctalia.homeModules.default alongside it double-declares the
    # option. Only the package still comes from the flake (via the overlay).
    home.programs.noctalia = {
      enable = true;
      package = pkgs.noctalia;
      systemd.enable = true;
      settings = noctaliaSettings;
    };
  };
}
