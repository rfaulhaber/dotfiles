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
    inherit (config.modules.themes) font;
    inherit (cfg) networkInterface;
    allowEmptyPassword = config.modules.services.yubikey.enable;
    inherit (config.userInfo) location;
    idleBehavior = cfg.idle.behavior;
    lockscreenOutputs = attrNames config.modules.desktop.environment.niri.outputs;
    vpn = {
      inherit (config.modules.services.airvpn) enable profile;
    };
    llm = {
      inherit (config.modules.services.llama-server) enable;
    };
    ime = {
      inherit (config.modules.desktop.inputMethod) enable layoutInputMethod;
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

    idle.behavior = mkOption {
      description = ''
        noctalia `[idle.behavior.*]` entries, keyed by behavior name. Keys
        beyond `action` and `timeout` (`enabled`, `locked_timeout`, `command`,
        `resume_command`) are forwarded verbatim. Declaring any entry replaces
        noctalia's built-in set; left empty, those defaults apply and every
        behavior is disabled.
      '';
      type = types.attrsOf (types.submodule {
        freeformType = types.attrsOf types.anything;
        # noctalia starts a declared entry from a blank record rather than the
        # same-named default, so a missing action or timeout makes it a silent
        # no-op instead of an error.
        options = {
          action = mkOption {
            description = "What the behavior does once its timeout elapses.";
            type = types.enum ["lock" "screen_off" "suspend" "lock_and_suspend" "command"];
          };
          timeout = mkOption {
            description = "Seconds without input before the action runs.";
            type = types.ints.positive;
          };
        };
      });
      default = {};
      example = literalExpression ''
        {
          lock = { enabled = true; action = "lock"; timeout = 600; };
          screen-off = { enabled = true; action = "screen_off"; timeout = 660; };
        }
      '';
    };
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
