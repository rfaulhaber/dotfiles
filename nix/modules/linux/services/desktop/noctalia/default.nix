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
    inherit lib;
    homePath = config.user.home;
    inherit (config.modules.themes) font;
    inherit (cfg) networkInterface;
    allowEmptyPassword = config.modules.services.yubikey.enable;
    inherit (config.userInfo) location;
    lockscreenOutputs = attrNames config.modules.desktop.environment.niri.outputs;
    vpn = {
      inherit (config.modules.services.airvpn) enable profile;
    };
    wallpaper = {
      inherit (cfg.wallpaper) enable;
      directory = config.modules.desktop.random-wallpaper.storeDir;
      tile = config.modules.desktop.random-wallpaper.enable;
    };
  };
in {
  options.modules.desktop.noctalia = {
    enable = mkEnableOption false;

    wallpaper.enable = lib.my.mkOptDesc types.bool false ''
      Let noctalia paint the wallpaper on its own background layer. awww is
      dropped, the random-wallpaper service drives noctalia over IPC instead,
      and the control center gains a tile that fetches a new wallpaper for the
      focused monitor.
    '';

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
